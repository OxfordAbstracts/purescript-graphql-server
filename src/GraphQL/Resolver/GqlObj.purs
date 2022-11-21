module GraphQL.Resolver.GqlObj where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Symbol (class IsSymbol, reflectSymbol)
import GraphQL.Server.Gql (class Gql, class GqlObject, objectWithName)
import Type.Proxy (Proxy(..))

data GqlObj :: Symbol -> Type -> Type
data GqlObj name a = GqlObj a

derive instance Generic (GqlObj name a) _

instance
  ( GqlObject (GqlObj name { | a })
  , IsSymbol name
  ) =>
  Gql (GqlObj name { | a }) where
  gql = objectWithName $ reflectSymbol (Proxy :: Proxy name)