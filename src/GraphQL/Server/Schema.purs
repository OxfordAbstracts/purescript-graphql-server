module GraphQL.Server.Schema where

import Prelude

import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (liftAff)
import GraphQL.Server.Gql (class Gql, getTypeWithoutNull)
import GraphQL.Server.GqlM (GqlM)
import GraphQL.Server.Introspection.GetTypes (getDescendantITypes)
import GraphQL.Server.Introspection.GqlNullable (class GqlNullable)
import GraphQL.Server.Introspection.Types (ISchema(..))
import GraphQL.Server.Resolver.Root (GqlRoot(..), MutationRoot(..), QueryRoot(..))
import Type.Proxy (Proxy(..))

class GetSchema env a where
  getSchema :: a -> GqlM env ISchema

instance
  ( Gql env (QueryRoot { | q })
  , GqlNullable (QueryRoot { | q })
  ) =>
  GetSchema env (GqlRoot (QueryRoot { | q }) (MutationRoot Unit)) where
  getSchema _ = do
    queryType <- getTypeWithoutNull (Proxy :: Proxy (QueryRoot { | q }))
    queryTypes <- liftAff $ getDescendantITypes queryType
    pure $ ISchema
      { types: queryTypes
      , queryType
      , mutationType: Nothing
      , subscriptionType: Nothing
      , directives: Nil
      }
else instance
  ( Gql env (QueryRoot { | q })
  , Gql env (MutationRoot { | m })
  ) =>
  GetSchema env (GqlRoot (QueryRoot { | q }) (MutationRoot { | m })) where
  getSchema _ = do
    queryType <- getTypeWithoutNull (Proxy :: Proxy (QueryRoot { | q }))
    mutationType <- getTypeWithoutNull (Proxy :: Proxy (MutationRoot { | m }))
    queryTypes <- liftAff $ getDescendantITypes queryType
    mutationTypes <- liftAff $ getDescendantITypes mutationType
    pure $ ISchema
      { types: queryTypes <> mutationTypes
      , queryType
      , mutationType: Just mutationType
      , subscriptionType: Nothing
      , directives: Nil
      }

test0 :: forall env. GqlM env ISchema
test0 = getSchema $ GqlRoot { query: QueryRoot { t: 1 }, mutation: MutationRoot unit }
