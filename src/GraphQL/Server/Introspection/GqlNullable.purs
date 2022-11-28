module GraphQL.Server.Introspection.GqlNullable where

import Data.Maybe (Maybe)
import Type.Proxy (Proxy)

class GqlNullable :: forall k. k -> Constraint
class GqlNullable a where
  isNullable :: Proxy a -> Boolean

instance GqlNullable (Maybe a) where
  isNullable _ = true
else instance GqlNullable a where
  isNullable _ = false