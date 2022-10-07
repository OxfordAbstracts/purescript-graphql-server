module GraphQL.Resolver.HandleQuery where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut (Json, encodeJson, jsonNull)
import Data.Either (Either(..))
import Data.GraphQL.AST (OperationType(..))
import Data.GraphQL.AST as AST
import Data.Map (lookup)
import Data.Maybe (Maybe(..), maybe)
import Data.String (toLower)
import Foreign.Object (Object)
import Foreign.Object as Object
import GraphQL.Resolver.Gqlable (class Gqlable)
import GraphQL.Resolver.JsonResolver (Resolver(..), resolve)
import GraphQL.Resolver.Result (Result, resultToData)
import GraphQL.Server.GqlError (GqlError(..))

handleOperationDefinition :: forall m f. Gqlable f m => Resolver f -> AST.OperationDefinition -> Object Json -> f (Either GqlError Json)
handleOperationDefinition resolver opDef vars = case opDef of
  AST.OperationDefinition_SelectionSet selectionSet ->
    getJson <$> resolve resolver Object.empty (Just selectionSet)
  AST.OperationDefinition_OperationType
    { operationType --  ∷ OperationType
    , variableDefinitions --  ∷ (Maybe VariableDefinitions)
    , directives --  ∷ (Maybe Directives)
    , selectionSet --  ∷ SelectionSet
    } -> case resolver of
    Node _ -> pure $ Left $ OtherError "Node resolver at root"
    ResolveAsync _ -> pure $ Left $ OtherError "ResolveAsync resolver at root"
    ListResolver _ -> pure $ Left $ OtherError "List resolver at root"
    FailedResolver err -> pure $ Left $ ResolverError err
    Fields { fields } ->
      (lookup rootField fields <|> lookup (toLower rootField) fields)
        # maybe (pure $ Left $ OtherError $ show rootField <> " not field found")
            \rootFields -> getJson <$> resolve (rootFields.resolver { args: jsonNull }) vars (Just selectionSet)
      where
      rootField = case operationType of
        Query -> "Query"
        Mutation -> "Mutation"
        Subscription -> "Subscription"
  where
  getJson :: forall e. Applicative e => Result -> e Json
  getJson result = pure $ encodeJson { data: resultToData result }
