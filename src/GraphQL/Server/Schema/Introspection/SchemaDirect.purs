module GraphQL.Server.Schema.Introspection.SchemaDirect where

import Prelude

import Data.List (List(..))
import Data.Maybe (Maybe(..))
import GraphQL.Resolver.Root (GqlRoot(..))
import GraphQL.Server.MaxDepth (MaxDepth)
import GraphQL.Server.Schema.Introspection.GetType (class GetIType, getIType)
import GraphQL.Server.Schema.Introspection.GetTypes (getDescendantITypes)
import GraphQL.Server.Schema.Introspection.Types (ISchema(..))
import Type.Proxy (Proxy(..))

class SchemaDirect a where
  schemaDirect :: a -> ISchema

instance (GetIType q) => SchemaDirect (GqlRoot q Unit) where
  schemaDirect _ = ISchema
    { types: getDescendantITypes queryType
    , queryType
    , mutationType: Nothing
    , subscriptionType: Nothing
    , directives: Nil
    }
    where
    queryType = getIType (Proxy :: Proxy q)

test0 :: ISchema
test0 = schemaDirect $ GqlRoot { query: 1, mutation: unit }

test1 :: ISchema
test1 = schemaDirect $ GqlRoot { query: [ 1 ], mutation: unit }
