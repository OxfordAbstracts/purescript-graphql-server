module GraphQL.Server.MaxDepth where

import Data.Typelevel.Num (D16, D4, D5, D6)
import Type.Proxy (Proxy(..))

-- The max depth of a query.
type MaxDepth = D16

-- The max depth of a query proxy for use in runtime logic.
maxDepth = Proxy :: Proxy MaxDepth

-- The max depth of a query.
type MaxIntrospectionDepth = D6

-- The max depth of a query proxy for use in runtime logic.
maxIntrospectionDepth = Proxy :: Proxy MaxIntrospectionDepth
