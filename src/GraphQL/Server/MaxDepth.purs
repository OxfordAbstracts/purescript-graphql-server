module GraphQL.Server.MaxDepth where

import Data.Typelevel.Num (D32)
import Type.Proxy (Proxy(..))

-- The max depth of a query.
type MaxDepth = D32

-- The max depth of a query proxy for use in runtime logic.
maxDepth = Proxy :: Proxy MaxDepth

-- The max depth of a query.
type MaxIntrospectionDepth = D32

-- The max depth of a query proxy for use in runtime logic.
maxIntrospectionDepth = Proxy :: Proxy MaxIntrospectionDepth
