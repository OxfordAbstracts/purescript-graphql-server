module GraphQL.Server.Schema where

import Prelude

import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Debug (spy)
import GraphQL.Server.Resolver.Root (GqlRoot(..), MutationRoot(..), QueryRoot(..))
import GraphQL.Server.Gql (class Gql, getTypeWithNull, getTypeWithoutNull)
import GraphQL.Server.Introspection.GetTypes (getDescendantITypes)
import GraphQL.Server.Introspection.GqlNullable (class GqlNullable)
import GraphQL.Server.Introspection.Types (ISchema(..))
import Type.Proxy (Proxy(..))

class GetSchema a where
  getSchema :: a -> ISchema

instance
  ( Gql (QueryRoot { | q })
  , GqlNullable (QueryRoot { | q })
  ) =>
  GetSchema (GqlRoot (QueryRoot { | q }) (MutationRoot Unit)) where
  getSchema _ = ISchema
    { types: getDescendantITypes queryType
    , queryType
    , mutationType: Nothing
    , subscriptionType: Nothing
    , directives: Nil
    }
    where
    queryType = getTypeWithoutNull (Proxy :: Proxy (QueryRoot { | q }))
else instance
  ( Gql (QueryRoot { | q })
  , Gql (MutationRoot { | m })
  ) =>
  GetSchema (GqlRoot (QueryRoot { | q }) (MutationRoot { | m })) where
  getSchema _ = ISchema
    { types: getDescendantITypes queryType <> getDescendantITypes mutationType
    , queryType
    , mutationType: Just mutationType
    , subscriptionType: Nothing
    , directives: Nil
    }
    where
    queryType = getTypeWithoutNull (Proxy :: Proxy (QueryRoot { | q }))
    mutationType = getTypeWithoutNull (Proxy :: Proxy (MutationRoot { | m }))

test0 :: ISchema
test0 = getSchema $ GqlRoot { query: QueryRoot { t: 1 }, mutation: MutationRoot unit }

test1 :: ISchema
test1 = getSchema $ GqlRoot { query: QueryRoot { t: [ 1 ] }, mutation: MutationRoot { x: "" } }
