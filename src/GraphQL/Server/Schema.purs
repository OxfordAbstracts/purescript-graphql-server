module GraphQL.Server.Schema where

import Prelude

import Data.List (List(..))
import Data.Maybe (Maybe(..))
import GraphQL.Server.Gql (class Gql, getTypeWithoutNull)
import GraphQL.Server.Introspection.GetTypes (getDescendantITypes)
import GraphQL.Server.Introspection.GqlNullable (class GqlNullable)
import GraphQL.Server.Introspection.Types (ISchema(..))
import GraphQL.Server.Resolver.Root (GqlRoot(..), MutationRoot(..), QueryRoot(..))
import Type.Proxy (Proxy(..))

class GetSchema env a where
  getSchema :: Proxy env -> a -> ISchema

instance
  ( Gql env (QueryRoot { | q })
  , GqlNullable (QueryRoot { | q })
  ) =>
  GetSchema env (GqlRoot (QueryRoot { | q }) (MutationRoot Unit)) where
  getSchema _ _ = ISchema
    { types: getDescendantITypes queryType
    , queryType
    , mutationType: Nothing
    , subscriptionType: Nothing
    , directives: Nil
    }
    where
    queryType = getTypeWithoutNull (Proxy :: Proxy env) (Proxy :: Proxy (QueryRoot { | q }))
else instance
  ( Gql env (QueryRoot { | q })
  , Gql env (MutationRoot { | m })
  ) =>
  GetSchema env (GqlRoot (QueryRoot { | q }) (MutationRoot { | m })) where
  getSchema _ _ = ISchema
    { types: getDescendantITypes queryType <> getDescendantITypes mutationType
    , queryType
    , mutationType: Just mutationType
    , subscriptionType: Nothing
    , directives: Nil
    }
    where
    queryType = getTypeWithoutNull (Proxy :: Proxy env) (Proxy :: Proxy (QueryRoot { | q }))
    mutationType = getTypeWithoutNull (Proxy :: Proxy env) (Proxy :: Proxy (MutationRoot { | m }))

test0 :: ISchema
test0 = getSchema Proxy $ GqlRoot { query: QueryRoot { t: 1 }, mutation: MutationRoot unit }

-- test1 :: ISchema
-- test1 = getSchema $ GqlRoot { query: QueryRoot { t: [ 1 ] }, mutation: MutationRoot { x: "" } }
