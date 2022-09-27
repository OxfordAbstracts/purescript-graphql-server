module GraphQL.Resolver.CombineRecordIo where

-- import Prelude

-- import Effect.Aff (Aff)
-- import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson)
-- import Data.Either (Either(..))
-- import Data.List (List(..))
-- import Data.Map (Map)
-- import Data.Map as Map
-- import Data.Newtype (class Newtype, unwrap)
-- import Data.Symbol (class IsSymbol, reflectSymbol)
-- import Effect.Aff (Aff)
-- import GraphQL.Resolver.GqlIo (GqlIo(..))
-- import GraphQL.Resolver.JsonResolver (Field, Resolver(..))
-- import GraphQL.Resolver.JsonResolver as JsonResolver
-- import GraphQL.Resolver.Resolver.ResolveTo (GqlObj(..))
-- import GraphQL.Resolver.Result (Result(..))
-- import GraphQL.Server.GqlError (ResolverError(..))
-- import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
-- import Type.Proxy (Proxy)
-- import Unsafe.Coerce (unsafeCoerce)


-- data CombineApp = CombineApp


-- instance
--   ( IsSymbol sym
--   , Applicative m 
--   ) =>
--   FoldingWithIndex CombineApp (Proxy sym) (FieldMap m) a (FieldMap m) where
--   foldingWithIndex CombineApp prop (FieldMap fieldMap) a =
--     FieldMap $ Map.insert name field fieldMap
--     where
--     name = reflectSymbol prop

--     field :: Field m
--     field =
--       { name
--       , resolver: getArgResolver a
--       }