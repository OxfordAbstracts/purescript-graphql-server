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
import GraphQL.Resolver.Gqlable (class Gqlable)
import GraphQL.Resolver.JsonResolver (Fields, Resolver(..), resolve)
import GraphQL.Resolver.Result (Result, resultToData)
import GraphQL.Resolver.ToResolver (getArgResolver, toResolver)
import GraphQL.Server.GqlError (GqlError(..))
import Unsafe.Coerce (unsafeCoerce)

handleOperationDefinition :: forall m f. Gqlable f m => Resolver f -> AST.OperationDefinition -> f (Either GqlError Json)
handleOperationDefinition resolver opDef = case opDef of
  AST.OperationDefinition_SelectionSet selectionSet ->
    getJson <$> resolve resolver (Just selectionSet)
  -- in getJson result
  AST.OperationDefinition_OperationType
    { operationType --  ∷ OperationType
    , variableDefinitions --  ∷ (Maybe VariableDefinitions)
    , directives --  ∷ (Maybe Directives)
    , selectionSet --  ∷ SelectionSet
    } -> case resolver of
    -- LazyResolver r -> handleOperationDefinition (r unit) opDef
    Node _ -> pure $ Left $ OtherError "Node resolver at root"
    ResolveAsync _ -> pure $ Left $ OtherError "ResolveAsync resolver at root"
    ListResolver _ -> pure $ Left $ OtherError "List resolver at root"
    FailedResolver err -> pure $ Left $ ResolverError err
    Fields { fields } ->
      (lookup rootField fields <|> lookup (toLower rootField) fields)
        # maybe (pure $ Left $ OtherError $ show rootField <> " not field found")
            \rootFields -> getJson <$> resolve (rootFields.resolver { args: jsonNull }) (Just selectionSet)
      where
      rootField = case operationType of
        Query -> "Query"
        Mutation -> "Mutation"
        Subscription -> "Subscription"

  where
  getJson :: forall e. Applicative e => Result -> e Json
  getJson result = pure $ encodeJson { data: resultToData result }
-- case operationType of 
-- Query -> 

-- handleQuery :: forall m a . Resolver m -> String -> m Json
-- handleQuery resolver query = do
--   result <- runResolver resolver query
--   case result of
--     Left err -> pure $ encodeJson err
--     Right json -> pure json