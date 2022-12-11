module GraphQL.Server.HandleOperation where

import Prelude

import Control.Monad.Reader as H
import Data.Argonaut (Json)
import Data.Either (Either(..))
import Data.GraphQL.AST (OperationType(..))
import Data.GraphQL.AST as AST
import Data.List.NonEmpty as NonEmpty
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..))
import GraphQL.Resolver (RootResolver)
import GraphQL.Server.GqlError (GqlError(..))
import GraphQL.Server.GqlM (GqlM)
import GraphQL.Server.Resolver.JsonResolver (resolve)
import GraphQL.Server.Resolver.Result (encodeLocatedError, getLocatedErrors, resultToData)

handleOperation
  :: forall env
   . GqlM env (RootResolver env)
  -> AST.OperationDefinition
  -> GqlM env
       ( Either GqlError
           { data :: Json
           , errors :: Maybe (NonEmptyList Json)
           }
       )
handleOperation rootM opDef = do
  root <- rootM
  { request } <- H.ask
  let
    mutation = root.mutation request
    query = root.query request

  case opDef of
    AST.OperationDefinition_SelectionSet selectionSet ->
      resolveToJson query selectionSet
    AST.OperationDefinition_OperationType
      { operationType
      , selectionSet
      } ->
      case operationType of
        Query -> resolveToJson query selectionSet
        Mutation -> resolveToJson mutation selectionSet
        Subscription -> pure $ Left $ SubscriptionsNotSupported
  where

  resolveToJson r selectionSet = getJson <$> resolve r (Just selectionSet)

  getJson result = pure
    { data: resultToData result
    , errors: NonEmpty.fromList $ encodeLocatedError <$> getLocatedErrors result
    }

