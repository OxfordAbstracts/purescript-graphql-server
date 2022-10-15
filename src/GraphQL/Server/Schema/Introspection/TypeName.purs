module GraphQL.Server.Schema.Introspection.TypeName where

import Data.Argonaut (Json)
import Data.List (List)
import Data.List.Lazy as Lazy
import Data.Maybe (Maybe)
import GraphQL.Resolver.Resolver.GqlObject (GqlObj)
import Prim.Symbol (class Append)

class GqlTypeName :: Type -> Symbol -> Constraint
class GqlTypeName a name | a -> name

instance GqlTypeName (GqlObj name a) name
instance GqlTypeName Boolean "Boolean!"
instance GqlTypeName Int "Int!"
instance GqlTypeName Number "Float!"
instance GqlTypeName String "String!"
instance GqlTypeName Json "Json!"

instance
  ( GqlTypeName a name
  , Append "[" name left
  , Append left "]!" result
  ) =>
  GqlTypeName (Array a) result

instance
  ( GqlTypeName a name
  , Append "[" name left
  , Append left "]!" result
  ) =>
  GqlTypeName (List a) result

instance
  ( GqlTypeName a name
  , Append "[" name left
  , Append left "]!" result
  ) =>
  GqlTypeName (Lazy.List a) result

instance
  ( GqlTypeName a name
  , Append result "!" name
  ) =>
  GqlTypeName (Maybe a) result
