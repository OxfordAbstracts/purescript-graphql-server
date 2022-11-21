module GraphQL.Server.HandleRequest (handleRequest, parseOperation) where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError)
import Control.Parallel (class Parallel)
import Data.Argonaut (Json, decodeJson, encodeJson, parseJson)
import Data.Either (Either(..), either, note)
import Data.Filterable (filterMap)
import Data.Foldable (findMap)
import Data.GraphQL.AST as AST
import Data.GraphQL.Parser (document)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Foreign.Object (Object)
import Foreign.Object as Object
import GraphQL.Resolver (RootResolver)
import GraphQL.Resolver.Error (class CustomResolverError)
import GraphQL.Resolver.EvalGql (class EvalGql, evalGql)
import GraphQL.Resolver.HandleOperation (handleOperation)
import GraphQL.Server.GqlError (GqlError(..))
import GraphQL.Server.GqlResM (GqlResM)
import HTTPure (Request, toString)
import Parsing (runParser)

handleRequest
  :: forall f m err
   . CustomResolverError err
  => MonadError err m
  => Parallel f m
  => EvalGql m
  => (Request -> Aff Boolean)
  -> RootResolver err m
  -> Request
  -> GqlResM Json
handleRequest isAuthorized resolvers req = do
  authorized <- liftAff $ isAuthorized req
  when (not authorized) $ throwError NotAuthorized
  bodyStr <- liftAff $ toString req.body
  { operationName, operation, variables } <- parseGqlRequest bodyStr
  op <- parseOperation operationName operation
  either throwError pure =<<
    ( liftAff
        $ evalGql req
        $ map (map encodeJson)
        $ handleOperation resolvers req op (fromMaybe Object.empty variables)
    )

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

-- parseOperation :: Maybe String -> String -> GqlResM AST.OperationDefinition
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

