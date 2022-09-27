module GraphQL.Server.Schema where

import Prelude

import Type.Proxy (Proxy)

class GqlSchema :: forall k. k -> Constraint
class GqlSchema a where
  gqlSchema :: Proxy a -> String

instance GqlSchema Boolean where
  gqlSchema _ = "Boolean"

instance GqlSchema Int where
  gqlSchema _ = "Int"

instance GqlSchema Number where
  gqlSchema _ = "Float"

instance GqlSchema String where
  gqlSchema _ = "String"

-- instance name :: Class Type