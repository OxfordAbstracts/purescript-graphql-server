module GraphQL.Server.HandleRequest where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Argonaut (Json, decodeJson, parseJson)
import Data.Either (Either(..), either, note)
import Data.Filterable (filterMap)
import Data.Foldable (findMap)
import Data.GraphQL.AST as AST
import Data.GraphQL.Parser (document)
import Data.List (List(..), foldM, (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff.Class (class MonadAff, liftAff)
import Foreign.Object (Object)
import Foreign.Object as Object
import GraphQL.Resolver.Gqlable (class Gqlable, toAff)
import GraphQL.Resolver.HandleQuery (handleOperationDefinition)
import GraphQL.Resolver.JsonResolver (Resolver)
import GraphQL.Server.Value.Encode (encodeValue)
import GraphQL.Server.GqlError (GqlError(..), VariableInputError(..))
import GraphQL.Server.GqlResM (GqlResM)
import HTTPure (Request, toString)
import Parsing (runParser)

handleRequest
  :: forall m f
   . Gqlable f m
  => Resolver f
  -> Request
  -> GqlResM Json
handleRequest resolvers req = do
  bodyStr <- liftAff $ toString req.body
  { operationName, operation, variables } <- parseGqlRequest bodyStr
  op <- parseOperation operationName operation
  either throwError pure =<< (liftAff $ toAff $ handleOperationDefinition resolvers op $ fromMaybe Object.empty variables)

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

makeVariables :: Object Json -> List AST.VariableDefinition -> Either VariableInputError (Object Json)
makeVariables vars = foldM makeVar Object.empty
  where
  makeVar :: Object Json -> AST.VariableDefinition -> Either VariableInputError (Object Json)
  makeVar coercedVars (AST.VariableDefinition varDef@{ variable: AST.Variable varName }) =
    case Object.lookup varName vars, varDef of

      Just var, _ -> Right $ Object.insert varName var coercedVars

      Nothing, { defaultValue: Just (AST.DefaultValue defaultValue) } ->
        Right
          $ Object.insert varName (encodeValue defaultValue) coercedVars

      _, _ -> Left $ VariableNotFound varName

