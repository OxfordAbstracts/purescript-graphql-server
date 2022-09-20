module GraphQL.Resolver.Typed where

-- import Prelude

-- import Data.Symbol (class IsSymbol, reflectSymbol)
-- import GraphQL.Resolver.Untyped (Resolver)
-- import Heterogeneous.Folding (class FoldingWithIndex)
-- import Heterogeneous.Mapping (class Mapping)
-- import Type.Proxy (Proxy(..))

-- class MakeResolver a m where
--   makeResolver :: a -> Resolver m

-- data MakeResolverAdt = MakeResolverAdt

-- instance
--   (Show a, IsSymbol sym) =>
--   FoldingWithIndex MakeResolverAdt (Proxy sym) (Resolver m) a (Resolver m) where
--   foldingWithIndex MakeResolverAdt prop str a =
--     pre <> reflectSymbol prop <> ": " <> show a
--     where
--     pre | str == "" = ""
--         | otherwise = str <> ", "


-- instance MakeResolver {|r} m where  
--   makeResolver rec = 