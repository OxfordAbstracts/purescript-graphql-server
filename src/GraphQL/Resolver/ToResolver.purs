module GraphQL.Resolver.ToResolver
  ( class ToResolver
  , class GetArgResolver
  , class ToResolverGeneric
  , toResolverGeneric
  , ToResolverProps(..)
  , FieldMap(..)
  , WithTypeName(..)
  , getArgResolver
  , toResolver
  , resolveNode
  , genericResolver
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
import GraphQL.Resolver.Resolver.GqlObject (GqlObj(..))
import GraphQL.Resolver.Root (GqlRoot(..))
import GraphQL.Server.GqlError (ResolverError(..))
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

class ToResolver a m where
  toResolver :: a -> JsonResolver.Resolver m

resolveNode :: forall f a. Applicative f => EncodeJson a => a -> Resolver f
resolveNode a = Node $ pure $ encodeJson a

resolveAsync :: forall f a. Functor f => ToResolver a f => f a -> Resolver f
resolveAsync a = ResolveAsync $ map toResolver a

instance (ToResolver a (GqlIo m), Applicative m) => ToResolver (GqlIo m a) (GqlIo m) where
  toResolver a = resolveAsync a

else instance Applicative m => ToResolver Boolean m where
  toResolver a = resolveNode a

else instance Applicative m => ToResolver Int m where
  toResolver a = resolveNode a

else instance Applicative m => ToResolver Number m where
  toResolver a = resolveNode a

else instance Applicative m => ToResolver String m where
  toResolver a = resolveNode a

else instance Applicative m => ToResolver Json m where
  toResolver a = resolveNode a

else instance Applicative m => ToResolver Unit m where
  toResolver a = resolveNode a

else instance Applicative m => ToResolver Void m where
  toResolver a = resolveNode a

else instance (Applicative m, ToResolver a m) => ToResolver (List a) m where
  toResolver a = ListResolver $ map toResolver a

else instance (Applicative m, ToResolver a m) => ToResolver (Maybe a) m where
  toResolver a = NullableResolver $ map toResolver a

else instance (Applicative m, ToResolver a m) => ToResolver (Array a) m where
  toResolver a = ListResolver $ map toResolver $ List.fromFoldable a

else instance ToResolver a m => ToResolver (Unit -> a) m where
  toResolver a = toResolver $ a unit

else instance
  ( Applicative m
  , HFoldlWithIndex (ToResolverProps m) (FieldMap m) { | r } (FieldMap m)
  , IsSymbol name
  ) =>
  ToResolver (GqlObj name { | r }) m where
  toResolver (GqlObj a) = toResolver (WithTypeName a :: WithTypeName name _)

else instance
  ( Applicative m
  , HFoldlWithIndex (ToResolverProps m) (FieldMap m) ({ query :: q, mutation :: mut }) (FieldMap m)
  , IsSymbol "root"
  ) =>
  ToResolver (GqlRoot q mut) m where
  toResolver (GqlRoot root) = toResolver (WithTypeName root :: WithTypeName "root" _)

else instance
  ( Applicative m
  , HFoldlWithIndex (ToResolverProps m) (FieldMap m) { | r } (FieldMap m)
  , IsSymbol name
  ) =>
  ToResolver (WithTypeName name { | r }) m where
  toResolver (WithTypeName a) = Fields
    { fields: makeFields (reflectSymbol (Proxy :: Proxy name)) a
    , typename: reflectSymbol (Proxy :: Proxy name)
    }

else instance
  ( Generic r rep
  , ToResolverGeneric rep m
  ) =>
  ToResolver r m where
  toResolver a = toResolverGeneric $ from a

class ToResolverGeneric a m where
  toResolverGeneric :: a -> JsonResolver.Resolver m

instance
  ( Applicative m
  -- , IsSymbol name
  , HFoldlWithIndex (ToResolverProps m) (FieldMap m) { | arg } (FieldMap m)
  ) =>
  ToResolverGeneric (Constructor name (Argument { | arg })) m where
  toResolverGeneric (Constructor (Argument arg)) = unsafeCoerce unit

-- Fields
--   { fields: makeFields (reflectSymbol (Proxy :: Proxy name)) arg
--   , typename: reflectSymbol (Proxy :: Proxy name)
--   }

newtype WithTypeName :: Symbol -> Type -> Type
newtype WithTypeName sym a = WithTypeName a

derive instance Newtype (WithTypeName sym a) _

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

genericResolver
  :: forall r name m a
   . Generic r (Constructor name (Argument a))
  => ToResolver (WithTypeName name a) m
  => r
  -> Resolver m
genericResolver a = toResolver $ (WithTypeName arg :: WithTypeName name a)
  where
  (Constructor (Argument arg)) = from a