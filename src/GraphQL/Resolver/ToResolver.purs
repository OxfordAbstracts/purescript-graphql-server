module GraphQL.Resolver.ToResolver
  ( class ToResolver
  , class GetArgResolver
  -- , class ToResolverCustom
  -- , toResolverCustom
  , ToResolverProps(..)
  , FieldMap(..)
  , getArgResolver
  , toResolver
  , resolveNode
  , objectResolver
  ) where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), from)
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (class IsSymbol, reflectSymbol)
import GraphQL.Resolver.GqlIo (GqlIo)
import GraphQL.Resolver.JsonResolver (Field, Resolver(..))
import GraphQL.Resolver.JsonResolver as JsonResolver
import GraphQL.Resolver.Root (GqlRoot(..))
import GraphQL.Server.GqlError (ResolverError(..))
-- import GraphQL.Server.Schema.Introspection.Types (ITypeKind)
-- import GraphQL.Server.Schema.Introspection.Types.DirectiveLocation (IDirectiveLocation)
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Type.Proxy (Proxy(..))

class ToResolver a m where
  toResolver :: a -> JsonResolver.Resolver m

resolveNode :: forall m a. Applicative m => EncodeJson a => a -> Resolver m
resolveNode a = Node $ pure $ encodeJson a

resolveAsync :: forall m a. Functor m => ToResolver a m => m a -> Resolver m
resolveAsync a = ResolveAsync $ toResolver <$> a

instance (ToResolver a (GqlIo m), Applicative m) => ToResolver (GqlIo m a) (GqlIo m) where
  toResolver a = resolveAsync a

instance (Applicative m) => ToResolver Boolean m where
  toResolver a = resolveNode a

instance (Applicative m) => ToResolver Int m where
  toResolver a = resolveNode a

instance (Applicative m) => ToResolver Number m where
  toResolver a = resolveNode a

instance (Applicative m) => ToResolver String m where
  toResolver a = resolveNode a

instance (Applicative m) => ToResolver Json m where
  toResolver a = resolveNode a

instance (Applicative m) => ToResolver Unit m where
  toResolver a = resolveNode a

instance (Applicative m) => ToResolver Void m where
  toResolver a = resolveNode a

-- instance (Applicative m) => ToResolver IDirectiveLocation m where
--   toResolver a = resolveNode a

-- instance (Applicative m) => ToResolver ITypeKind m where
--   toResolver a = resolveNode a

instance (Applicative m, ToResolver a m) => ToResolver (List a) m where
  toResolver a = ListResolver $ toResolver <$> a

instance (Applicative m, ToResolver a m) => ToResolver (Maybe a) m where
  toResolver a = NullableResolver $ toResolver <$> a

instance (Applicative m, ToResolver a m) => ToResolver (Array a) m where
  toResolver a = ListResolver $ map (toResolver) $ List.fromFoldable a

instance ToResolver a m => ToResolver (Unit -> a) m where
  toResolver a = toResolver $ a unit

-- instance
--   Applicative m =>
--   ToResolver D0 a m where
--   toResolver _ _ = FailedResolver MaximumDepthExceeded

instance
  ( Applicative m
  , HFoldlWithIndex (ToResolverProps m) (FieldMap m) ({ query :: q, mutation :: mut }) (FieldMap m)
  , IsSymbol "root"
  ) =>
  ToResolver (GqlRoot q mut) m where
  toResolver (GqlRoot root) = Fields
    { fields: makeFields (reflectSymbol (Proxy :: Proxy "root")) root
    , typename: "root"
    }

objectResolver
  :: forall m a arg name
   . Applicative m
  => Generic a (Constructor name (Argument { | arg }))
  => IsSymbol name
  => HFoldlWithIndex (ToResolverProps m) (FieldMap m) { | arg } (FieldMap m)
  => a
  -> Resolver m
objectResolver = from >>> \(Constructor (Argument arg)) ->
  Fields
    { fields: makeFields (reflectSymbol (Proxy :: Proxy name)) arg
    , typename: reflectSymbol (Proxy :: Proxy name)
    }

-- instance
--   ( Generic r rep
--   , ToResolverCustom rep m
--   ) =>
--   ToResolver r m where
--   toResolver a = toResolverCustom $ from a

-- class ToResolverCustom a m where
--   toResolverCustom :: a -> JsonResolver.Resolver m

-- instance
--   ( Applicative m
--   , Nat nat
--   , Succ pred nat
--   , IsSymbol name
--   , HFoldlWithIndex (ToResolverProps m) (FieldMap m) { | arg } (FieldMap m)
--   ) =>
--   ToResolverCustom (Constructor name (Argument { | arg })) m where
--   toResolverCustom (Constructor (Argument arg)) =
-- Fields
--   { fields: makeFields (reflectSymbol (Proxy :: Proxy name)) arg
--   , typename: reflectSymbol (Proxy :: Proxy name)
--   }

makeFields
  :: forall r m
   . HFoldlWithIndex (ToResolverProps m) (FieldMap m) { | r } (FieldMap m)
  => Applicative m
  => String
  -> { | r }
  -> Map String (Field m)
makeFields typename r =
  unwrap $ ((hfoldlWithIndex (ToResolverProps :: ToResolverProps m) resolveTypename r) :: FieldMap m)
  where
  resolveTypename :: FieldMap m
  resolveTypename = FieldMap $ Map.singleton "__typename"
    { name: "__typename"
    , resolver: \_ -> resolveNode typename
    }

data ToResolverProps :: forall k. k -> Type
data ToResolverProps m = ToResolverProps

newtype FieldMap m = FieldMap (Map String (Field m))

derive instance Newtype (FieldMap m) _

instance
  ( IsSymbol sym
  , GetArgResolver a m
  ) =>
  FoldingWithIndex (ToResolverProps m) (Proxy sym) (FieldMap m) a (FieldMap m) where
  foldingWithIndex ToResolverProps prop (FieldMap fieldMap) a =
    FieldMap $ Map.insert name field fieldMap
    where
    name = reflectSymbol prop

    field :: Field m
    field =
      { name
      , resolver: getArgResolver a
      }

class GetArgResolver a m where
  getArgResolver
    :: a
    -> { args :: Json }
    -> JsonResolver.Resolver m

instance argResolverUnitFn :: ToResolver a m => GetArgResolver (Unit -> a) m where
  getArgResolver a = \_ -> toResolver (a unit)

else instance argResolverAllFn :: (DecodeJson a, ToResolver b m) => GetArgResolver (a -> b) m where
  getArgResolver fn { args } = case decodeJson args of
    Left err -> FailedResolver $ ResolverDecodeError err
    Right a -> toResolver $ fn a

else instance argResolverAny :: ToResolver a m => GetArgResolver a m where
  getArgResolver a = \_ -> toResolver a
