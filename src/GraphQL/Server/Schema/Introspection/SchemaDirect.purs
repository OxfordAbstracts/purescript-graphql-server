module GraphQL.Server.Schema.Introspection.SchemaDirect where

import Prelude

import Data.Generic.Rep (class Generic, from)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import GraphQL.Resolver.Root (GqlRoot(..))
import GraphQL.Server.Schema.Introspection.Types (ISchema(..), IType(..), IType_T, defaultIType)
import GraphQL.Server.Schema.Introspection.Types as IT
import GraphQL.Server.Schema.Introspection.GetType
import GraphQL.Server.Schema.Introspection.GetTypes
import Safe.Coerce (coerce)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

class SchemaDirect a where
  schemaDirect :: a -> ISchema

instance (GetIType q) => SchemaDirect (GqlRoot q Unit) where
  schemaDirect (GqlRoot {}) = ISchema
    { types: getITypes (Proxy :: Proxy q)
    , queryType: getIType (Proxy :: Proxy q)
    , mutationType: Nothing --  Maybe IType
    , subscriptionType: Nothing --  Maybe IType
    , directives: Nil --  List IDirective
    }


test0 :: ISchema
test0 = schemaDirect $ GqlRoot { query: 1, mutation: unit }

test1 :: ISchema
test1 = schemaDirect $ GqlRoot { query: [ 1 ], mutation: unit }