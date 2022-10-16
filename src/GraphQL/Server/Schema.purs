module GraphQL.Server.Schema where

import Prelude

import Data.List (List(..))
import Data.Maybe (Maybe(..))
import GraphQL.Resolver.Root (GqlRoot(..), MutationRoot, QueryRoot)
import GraphQL.Server.Schema.Introspection.GetType (class GetIType, getIType)
import GraphQL.Server.Schema.Introspection.GetTypes (getDescendantITypes)
import GraphQL.Server.Schema.Introspection.Types (ISchema(..))
import Type.Proxy (Proxy(..))

class GetSchema a where
  getSchema :: a -> ISchema

instance (GetIType (QueryRoot {|q})) => GetSchema (GqlRoot {|q} Unit) where
  getSchema _ = ISchema
    { types: getDescendantITypes queryType
    , queryType
    , mutationType: Nothing
    , subscriptionType: Nothing
    , directives: Nil
    }
    where
    queryType = getIType (Proxy :: Proxy (QueryRoot {|q}))
else instance (GetIType (QueryRoot {|q}), GetIType (MutationRoot {|m})) => GetSchema (GqlRoot {|q} {|m}) where
  getSchema _ = ISchema
    { types: getDescendantITypes queryType <> getDescendantITypes mutationType
    , queryType
    , mutationType: Just mutationType
    , subscriptionType: Nothing
    , directives: Nil
    }
    where
    queryType = getIType (Proxy :: Proxy (QueryRoot {|q}))
    mutationType = getIType (Proxy :: Proxy (MutationRoot {|m}))

test0 :: ISchema
test0 = getSchema $ GqlRoot { query: {t: 1}, mutation: unit }

test1 :: ISchema
test1 = getSchema $ GqlRoot { query: {t: [ 1 ]}, mutation: {x: ""} }
