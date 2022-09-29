module GraphQL.Server.Schema where

import Prelude

import Data.GraphQL.AST.Print (class PrintAst)
import Type.Proxy (Proxy)

class PrintAst s <= GqlSchema a s where
  gqlSchema :: Proxy a -> s

-- instance GqlSchema Boolean where
--   gqlSchema _ = "Boolean"

-- instance GqlSchema Int where
--   gqlSchema _ = "Int"

-- instance GqlSchema Number where
--   gqlSchema _ = "Float"

-- instance GqlSchema String where
--   gqlSchema _ = "String"

-- instance name :: Class Type