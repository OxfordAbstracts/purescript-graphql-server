module GraphQL.Resolver.HandleQuery where

import Prelude

import Data.Argonaut (Json, encodeJson, jsonNull)
import Data.Either (Either(..))
import Data.Map (lookup)
import Data.Maybe (Maybe(..), maybe)
import GraphQL.Parser.AST (OperationType(..))
import GraphQL.Parser.AST as AST
import GraphQL.Resolver.JsonResolver (Fields, Resolver(..), resolve)
import GraphQL.Resolver.Result (Result, resultToData)
import GraphQL.Resolver.ToResolver (getArgResolver, toResolver)
import GraphQL.Server.GqlError (GqlError(..))
import Unsafe.Coerce (unsafeCoerce)

handleOperationDefinition :: forall m. Applicative m => Resolver m -> AST.OperationDefinition -> m (Either GqlError Json)
handleOperationDefinition resolver = case _ of
  AST.OperationDefinition_SelectionSet selectionSet ->
    getJson <$> resolve resolver (Just selectionSet)
  -- in getJson result
  AST.OperationDefinition_OperationType
    { operationType --  ∷ OperationType
    , variableDefinitions --  ∷ (Maybe VariableDefinitions)
    , directives --  ∷ (Maybe Directives)
    , selectionSet --  ∷ SelectionSet
    } -> case resolver of
    Node _ -> pure $ Left $ OtherError "Node resolver at root"
    ListResolver _ -> pure $ Left $ OtherError "List resolver at root"
    FailedResolver err -> pure $ Left $ ResolverError err
    Fields { fields } ->
      lookup rootField fields
        # maybe (pure $ Left $ OtherError $ show rootField <> " not field found")
            \rootFields -> getJson <$> resolve (rootFields.resolver { args: jsonNull }) (Just selectionSet)
      -- (?D $ resolve rootFields  (?d )) :: m (Either GqlError Json)
      -- getJson <$> ?D <$>
      --  (?d $ resolve resolver  (rootFields)) :: m (Either GqlError Json)
      --  <$> resolver rootFields
      --  $ getArgResolver rootFields
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