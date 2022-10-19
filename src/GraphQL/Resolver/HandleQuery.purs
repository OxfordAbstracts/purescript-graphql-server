module GraphQL.Resolver.HandleQuery where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut (Json, encodeJson, jsonNull)
import Data.Either (Either(..))
import Data.GraphQL.AST (OperationType(..))
import Data.GraphQL.AST as AST
import Data.List.NonEmpty as NonEmpty
import Data.List.Types (NonEmptyList)
import Data.Map (lookup)
import Data.Maybe (Maybe(..), maybe)
import Data.String (toLower)
import Foreign.Object (Object)
import Foreign.Object as Object
import GraphQL.Resolver (RootResolver)
import GraphQL.Resolver.Gqlable (class Gqlable)
import GraphQL.Resolver.JsonResolver (Resolver(..), resolve)
import GraphQL.Resolver.Result (Result, encodeLocatedError, getLocatedErrors, resultToData)
import GraphQL.Server.GqlError (GqlError(..))

handleOperationDefinition
  :: forall m f
   . Gqlable f m
  => RootResolver f
  -> AST.OperationDefinition
  -> Object Json
  -> f
       ( Either GqlError
           { data :: Json
           , errors :: Maybe (NonEmptyList Json)
           }
       )
handleOperationDefinition { mutation, query } opDef vars = case opDef of
  AST.OperationDefinition_SelectionSet selectionSet ->
    getJson <$> resolve query vars (Just selectionSet)
  AST.OperationDefinition_OperationType
    { operationType 
    -- , variableDefinitions --  ∷ (Maybe VariableDefinitions)
    -- , directives --  ∷ (Maybe Directives)
    , selectionSet 
    } -> case operationType of
    Query -> getJson <$> resolve query vars (Just selectionSet)
    Mutation -> getJson <$> resolve mutation vars (Just selectionSet)
    Subscription -> pure $ Left $ SubscriptionsNotSupported
  where
  getJson result = pure
    { data: resultToData result
    , errors: NonEmpty.fromList $ encodeLocatedError <$> getLocatedErrors result
    }
