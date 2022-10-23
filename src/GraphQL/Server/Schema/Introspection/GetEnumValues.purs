module GraphQL.Server.Schema.Introspection.GetEnumValues where

import Prelude

import Data.Generic.Rep (Constructor, NoArguments, Sum)
import Data.List (List)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Type.Proxy (Proxy(..))

class GetEnumValues :: forall k. k -> Constraint
class GetEnumValues a where
  getEnumValues :: Proxy a -> List String

instance IsSymbol name => GetEnumValues (Constructor name NoArguments) where
  getEnumValues _ = pure $ reflectSymbol (Proxy :: Proxy name)

instance (GetEnumValues a, GetEnumValues b) => GetEnumValues (Sum a b) where
  getEnumValues _ = getEnumValues (Proxy :: Proxy a) <> getEnumValues (Proxy :: Proxy b)
