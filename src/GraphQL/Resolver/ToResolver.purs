module GraphQL.Resolver.ToResolver
  ( FieldMap(..)
  , ToResolverProps(..)
  , class GetArgResolver
  , class ToResolver
  , class GenericUnionResolver
  , toObjectResolver
  , toScalarResolver
  , toEnumResolver
  , toUnionResolver
  , toResolver
  , genericUnionResolver
  , getArgResolver
  , makeFields
  ) where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson)
import Data.Argonaut.Encode.Generic (class EncodeLiteral, encodeLiteralSum)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), Sum(..), from)
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
import GraphQL.Server.GqlError (ResolverError(..))
import GraphQL.Server.GqlRep (class GqlRep, class Scalar, GEnum, GObject, GUnion, encodeScalar)
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Prim.Symbol (class Append)
import Type.Proxy (Proxy(..))

class ToResolver a m | m -> m where
  toResolver :: a -> JsonResolver.Resolver m

toScalarResolver
  :: forall m a name
   . Scalar a name
  => Applicative m
  => EncodeJson a
  => a
  -> Resolver m
toScalarResolver = unsafeResolveNodeWith encodeScalar

toEnumResolver
  :: forall m a rep name
   . Applicative m
  => Generic a rep
  => EncodeLiteral rep
  => GqlRep a GEnum name
  => a
  -> Resolver m
toEnumResolver = unsafeResolveNodeWith encodeLiteralSum

toObjectResolver
  :: forall m a arg name ctrName
   . Applicative m
  => Generic a (Constructor ctrName (Argument { | arg }))
  => GqlRep a GObject name
  => IsSymbol name
  => HFoldlWithIndex (ToResolverProps m) (FieldMap m) { | arg } (FieldMap m)
  => a
  -> Resolver m
toObjectResolver = from >>> unsafeToObjectResolverRep (Proxy :: Proxy name)

unsafeToObjectResolverRep
  :: forall m arg name ctrName
   . Applicative m
  => IsSymbol name
  => HFoldlWithIndex (ToResolverProps m) (FieldMap m) { | arg } (FieldMap m)
  => Proxy name
  -> (Constructor ctrName (Argument { | arg }))
  -> Resolver m
unsafeToObjectResolverRep _ (Constructor (Argument arg)) =
  Fields
    { fields: makeFields (reflectSymbol (Proxy :: Proxy name)) arg
    , typename: reflectSymbol (Proxy :: Proxy name)
    }

toUnionResolver
  :: forall m a rep name
   . Applicative m
  => Generic a rep
  => GenericUnionResolver name rep m
  => GqlRep a GUnion name
  => a
  -> Resolver m
toUnionResolver a = genericUnionResolver (Proxy :: Proxy name) $ from a

unsafeResolveNodeWith
  :: forall a m
   . Applicative m
  => (a -> Json)
  -> a
  -> Resolver m
unsafeResolveNodeWith encode a = Node $ pure $ encode a

unsafeResolveNode :: forall m a. Applicative m => EncodeJson a => a -> Resolver m
unsafeResolveNode a = Node $ pure $ encodeJson a

toAsyncResolver :: forall m a. Functor m => ToResolver a m => m a -> Resolver m
toAsyncResolver a = ResolveAsync $ toResolver <$> a

instance (ToResolver a (GqlIo m), Functor m) => ToResolver (GqlIo m a) (GqlIo m) where
  toResolver a = toAsyncResolver a

instance (Applicative m) => ToResolver Boolean m where
  toResolver a = unsafeResolveNode a

instance (Applicative m) => ToResolver Int m where
  toResolver a = unsafeResolveNode a

instance (Applicative m) => ToResolver Number m where
  toResolver a = unsafeResolveNode a

instance (Applicative m) => ToResolver String m where
  toResolver a = unsafeResolveNode a

instance (Applicative m) => ToResolver Json m where
  toResolver a = unsafeResolveNode a

instance (Applicative m) => ToResolver Unit m where
  toResolver a = unsafeResolveNode a

instance (Applicative m) => ToResolver Void m where
  toResolver a = unsafeResolveNode a

instance (Applicative m, ToResolver a m) => ToResolver (List a) m where
  toResolver a = ListResolver $ toResolver <$> a

instance (Applicative m, ToResolver a m) => ToResolver (Maybe a) m where
  toResolver a = NullableResolver $ toResolver <$> a

instance (Applicative m, ToResolver a m) => ToResolver (Array a) m where
  toResolver a = ListResolver $ map (toResolver) $ List.fromFoldable a

instance ToResolver a m => ToResolver (Unit -> a) m where
  toResolver a = toResolver $ a unit

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
    , resolver: \_ -> unsafeResolveNode typename
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

class GetArgResolver a m | m -> m where
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

class GenericUnionResolver :: Symbol -> Type -> (Type -> Type) -> Constraint
class GenericUnionResolver sym rep m where
  genericUnionResolver :: Proxy sym -> rep -> JsonResolver.Resolver m

instance
  ( GenericUnionResolver sym a m
  , GenericUnionResolver sym b m
  ) =>
  GenericUnionResolver sym (Sum a b) m where
  genericUnionResolver sym (Inl a) = genericUnionResolver sym a
  genericUnionResolver sym (Inr b) = genericUnionResolver sym b

instance
  ( Applicative m
  , IsSymbol fullName
  , HFoldlWithIndex (ToResolverProps m) (FieldMap m) { | arg } (FieldMap m)
  , Append sym name fullName
  ) =>
  GenericUnionResolver sym (Constructor name (Argument { | arg })) m where
  genericUnionResolver _sym a = unsafeToObjectResolverRep (Proxy :: Proxy fullName) a
else instance
  ( ToResolver arg m
  ) =>
  GenericUnionResolver _sym (Constructor name (Argument arg)) m where
  genericUnionResolver _sym (Constructor (Argument arg)) = toResolver arg