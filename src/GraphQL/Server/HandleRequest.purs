module GraphQL.Server.HandleRequest (handleRequest, parseOperation) where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Argonaut (Json, decodeJson, encodeJson, parseJson)
import Data.Either (Either(..), either, note)
import Data.Filterable (filterMap)
import Data.Foldable (findMap, null)
import Data.GraphQL.AST as AST
import Data.GraphQL.Parser (document)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Foreign.Object (Object)
import Foreign.Object as Object
import GraphQL.Resolver (RootResolver)
import GraphQL.Server.CoerceVars (coerceVars)
import GraphQL.Server.GqlError (GqlError(..))
import GraphQL.Server.GqlM (GqlM, runGqlM)
import GraphQL.Server.GqlResM (GqlResM)
import GraphQL.Server.HandleOperation (handleOperation)
import GraphQL.Server.Introspection (Introspection(..))
import HTTPure (Request, toString)
import Parsing (runParser)

handleRequest
  :: forall env
   . (Request -> Aff Boolean)
  -> (Request -> Aff env)
  -> GqlM env (RootResolver env)
  -> Request
  -> GqlResM Json
handleRequest isAuthorized mkEnv resolvers req = do
  authorized <- liftAff $ isAuthorized req
  when (not authorized) $ throwError NotAuthorized
  bodyStr <- liftAff $ toString req.body
  { operationName, operation, variables } <- parseGqlRequest bodyStr
  op <- parseOperation operationName operation
  let
    varDefs = case op of
      AST.OperationDefinition_OperationType
        { variableDefinitions: Just (AST.VariableDefinitions variableDefinitions)
        } -> variableDefinitions
      _ ->
        Nil
    rawVars = fromMaybe Object.empty variables

  coercedVars <-
    if null varDefs then
      pure Object.empty
    else do
      { introspection: Introspection introspection } <- toGqlM Object.empty resolvers
      case coerceVars introspection varDefs rawVars of
        Left err -> throwError $ VariableInputError err
        Right vars -> pure vars

  either throwError pure =<<
    ( toGqlM coercedVars
        $ map encodeJson
            <$> handleOperation resolvers op
    )

  where
  toGqlM :: forall a. Object Json -> GqlM env a -> GqlResM a
  toGqlM vars' a = liftAff $ runGqlM mkEnv req vars' a

parseGqlRequest
  :: forall m
   . MonadThrow GqlError m
  => String
  -> m
       { operationName :: Maybe String
       , operation :: String
       , variables :: Maybe (Object Json)
       }
parseGqlRequest =
  (parseJson >=> decodeJson) >>>
    either (ParseGqlRequestError >>> throwError) pure

parseOperation
  :: forall m
   . MonadAff m
  => MonadThrow GqlError m
  => Maybe String
  -> String
  -> m AST.OperationDefinition
parseOperation operationName body = do
  doc <- getDoc body
  either throwError pure $ getOperationDefinition operationName doc

getDoc
  :: forall m
   . MonadAff m
  => MonadThrow GqlError m
  => String
  -> m AST.Document
getDoc str = do
  case runParser str document of
    Left err -> throwError $ ParseGqlDocumentError err
    Right doc -> pure doc

getOperationDefinition :: Maybe String -> AST.Document -> Either GqlError (AST.OperationDefinition)
getOperationDefinition operationName (AST.Document doc) = case operationName of
  Nothing ->
    doc
      # filterMap case _ of
          AST.Definition_ExecutableDefinition (AST.ExecutableDefinition_OperationDefinition opDef) -> pure opDef
          _ -> Nothing
      # case _ of
          Nil -> Left NoOperationDefinition
          opDef : Nil -> pure opDef
          _ -> Left $ MultipleOperationDefinitions
  Just name ->
    doc
      # findMap case _ of
          AST.Definition_ExecutableDefinition (AST.ExecutableDefinition_OperationDefinition opDef) -> pure opDef
          _ -> Nothing
      # note (NoOperationDefinitionWithGivenName name)

