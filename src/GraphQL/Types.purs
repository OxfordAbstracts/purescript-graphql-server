module GraphQL.Types where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Symbol (class IsSymbol)
import GraphQL.Resolver.ToResolver (class ToResolver, FieldMap, ToResolverProps, toObjectResolver)
import GraphQL.Server.GqlRep (class GqlRep, GObject)
import GraphQL.Server.Schema.Introspection.GetType (class GetGqlType, class GetIFields, getObjectType)
import Heterogeneous.Folding (class HFoldlWithIndex)

-- make types with a needed type classes for usability

newtype Object :: Symbol -> Type -> Type
newtype Object n t = Object t

derive instance Generic (Object n t) _

instance GqlRep (Object n t) GObject n

instance
  ( Applicative m
  , HFoldlWithIndex (ToResolverProps err m) (FieldMap err m) { | a } (FieldMap err m)
  , IsSymbol n
  ) =>
  ToResolver err (Object n { | a }) m where
  toResolver a = toObjectResolver a

instance (GetIFields { | r }, IsSymbol n) => GetGqlType (Object n { | r }) where
  getType a = getObjectType a


