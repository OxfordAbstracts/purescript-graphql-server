module GraphQL.Resolver.ToResolver
  ( FieldMap(..)
  , ToResolverProps(..)
  , class GenericUnionResolver
  , class GetArgResolver
  , class ToResolver
  , genericUnionResolver
  , getArgResolver
  , makeFields
  , toEnumResolver
  , toObjectResolver
  , toResolver
  , toScalarResolver
  , toUnionResolver
  ) where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson)
import Data.Argonaut.Encode.Generic (class EncodeLiteral, encodeLiteralSum)
import Data.Date (Date)
import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), Sum(..), from)
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Time (Time)
import GraphQL.Resolver.GqlIo (GqlIo)
import GraphQL.Resolver.JsonResolver (Field, Resolver(..))
import GraphQL.Resolver.JsonResolver as JsonResolver
import GraphQL.Server.DateTime (encodeDate, encodeDateTime, encodeTime)
import GraphQL.Server.GqlError (FailedToResolve(..))
import GraphQL.Server.GqlRep (class GqlRep, GEnum, GObject, GUnion)
import GraphQL.Server.Schema.Scalar (class Scalar, encodeScalar)
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Prim.Symbol (class Append)
import Type.Proxy (Proxy(..))

class ToResolver err a m | m -> m, m -> err where
  toResolver :: a -> JsonResolver.Resolver err m

toScalarResolver
  :: forall m a name err
   . Scalar a name
  => Applicative m
  => a
  -> Resolver err m
toScalarResolver = unsafeResolveNodeWith encodeScalar

toEnumResolver
  :: forall m a rep name err
   . Applicative m
  => Generic a rep
  => EncodeLiteral rep
  => GqlRep a GEnum name
  => a
  -> Resolver err m
toEnumResolver = unsafeResolveNodeWith encodeLiteralSum

toObjectResolver
  :: forall m a arg name ctrName err
   . Applicative m
  => Generic a (Constructor ctrName (Argument { | arg }))
  => GqlRep a GObject name
  => IsSymbol name
  => HFoldlWithIndex (ToResolverProps err m) (FieldMap err m) { | arg } (FieldMap err m)
  => a
  -> Resolver err m
toObjectResolver = from >>> unsafeToObjectResolverRep (Proxy :: Proxy name)

unsafeToObjectResolverRep
  :: forall m arg name ctrName err
   . Applicative m
  => IsSymbol name
  => HFoldlWithIndex (ToResolverProps err m) (FieldMap err m) { | arg } (FieldMap err m)
  => Proxy name
  -> (Constructor ctrName (Argument { | arg }))
  -> Resolver err m
unsafeToObjectResolverRep _ (Constructor (Argument arg)) =
  Fields
    { fields: makeFields (reflectSymbol (Proxy :: Proxy name)) arg
    , typename: reflectSymbol (Proxy :: Proxy name)
    }

toUnionResolver
  :: forall m a rep name err
   . Applicative m
  => Generic a rep
  => GenericUnionResolver err name rep m
  => GqlRep a GUnion name
  => a
  -> Resolver err m
toUnionResolver a = genericUnionResolver (Proxy :: Proxy name) $ from a

unsafeResolveNodeWith
  :: forall a m err
   . Applicative m
  => (a -> Json)
  -> a
  -> Resolver err m
unsafeResolveNodeWith encode a = Node $ pure $ encode a

unsafeResolveNode :: forall err m a. Applicative m => EncodeJson a => a -> Resolver err m
unsafeResolveNode a = Node $ pure $ encodeJson a

toAsyncResolver :: forall err m a. Functor m => ToResolver err a m => m a -> Resolver err m
toAsyncResolver a = ResolveAsync $ toResolver <$> a

instance (ToResolver err a (GqlIo m), Functor m) => ToResolver err (GqlIo m a) (GqlIo m) where
  toResolver a = toAsyncResolver a

instance (Applicative m) => ToResolver err Boolean m where
  toResolver a = unsafeResolveNode a

instance (Applicative m) => ToResolver err Int m where
  toResolver a = unsafeResolveNode a

instance (Applicative m) => ToResolver err Number m where
  toResolver a = unsafeResolveNode a

instance (Applicative m) => ToResolver err String m where
  toResolver a = unsafeResolveNode a

instance (Applicative m) => ToResolver err Date m where
  toResolver a = unsafeResolveNodeWith encodeDate a

instance (Applicative m) => ToResolver err Time m where
  toResolver a = unsafeResolveNodeWith encodeTime a

instance (Applicative m) => ToResolver err DateTime m where
  toResolver a = unsafeResolveNodeWith encodeDateTime a

instance (Applicative m) => ToResolver err Json m where
  toResolver a = unsafeResolveNode a

instance (Applicative m) => ToResolver err Unit m where
  toResolver a = unsafeResolveNode a

instance (Applicative m) => ToResolver err Void m where
  toResolver a = unsafeResolveNode a

instance (Applicative m, ToResolver err a m) => ToResolver err (List a) m where
  toResolver a = ListResolver $ toResolver <$> a

instance (Applicative m, ToResolver err a m) => ToResolver err (Maybe a) m where
  toResolver a = NullableResolver $ toResolver <$> a

instance (Applicative m, ToResolver err a m) => ToResolver err (Array a) m where
  toResolver a = ListResolver $ map (toResolver) $ List.fromFoldable a

instance ToResolver err a m => ToResolver err (Unit -> a) m where
  toResolver a = toResolver $ a unit

makeFields
  :: forall r m err
   . HFoldlWithIndex (ToResolverProps err m) (FieldMap err m) { | r } (FieldMap err m)
  => Applicative m
  => String
  -> { | r }
  -> Map String (Field err m)
makeFields typename r =
  unwrap $ ((hfoldlWithIndex (ToResolverProps :: ToResolverProps err m) resolveTypename r) :: FieldMap err m)
  where
  resolveTypename :: FieldMap err m
  resolveTypename = FieldMap $ Map.singleton "__typename"
    { name: "__typename"
    , resolver: \_ -> unsafeResolveNode typename
    }

data ToResolverProps :: forall k1 k2. k1 -> k2 -> Type
data ToResolverProps err m = ToResolverProps

newtype FieldMap err m = FieldMap (Map String (Field err m))

derive instance Newtype (FieldMap err m) _

instance
  ( IsSymbol sym
  , GetArgResolver err a m
  ) =>
  FoldingWithIndex (ToResolverProps err m) (Proxy sym) (FieldMap err m) a (FieldMap err m) where
  foldingWithIndex ToResolverProps prop (FieldMap fieldMap) a =
    FieldMap $ Map.insert name field fieldMap
    where
    name = reflectSymbol prop

    field :: Field err m
    field =
      { name
      , resolver: getArgResolver a
      }

class GetArgResolver err a m | m -> m where
  getArgResolver
    :: a
    -> { args :: Json }
    -> JsonResolver.Resolver err m

instance argResolverUnitFn :: ToResolver err a m => GetArgResolver err (Unit -> a) m where
  getArgResolver a = \_ -> toResolver (a unit)

else instance argResolverAllFn :: (DecodeJson a, ToResolver err b m) => GetArgResolver err (a -> b) m where
  getArgResolver fn { args } = case decodeJson args of
    Left err -> FailedResolver $ ResolverDecodeError err
    Right a -> toResolver $ fn a

else instance argResolverAny :: ToResolver err a m => GetArgResolver err a m where
  getArgResolver a = \_ -> toResolver a

class GenericUnionResolver :: Type -> Symbol -> Type -> (Type -> Type) -> Constraint
class GenericUnionResolver err sym rep m where
  genericUnionResolver :: Proxy sym -> rep -> JsonResolver.Resolver err m

instance
  ( GenericUnionResolver err sym a m
  , GenericUnionResolver err sym b m
  ) =>
  GenericUnionResolver err sym (Sum a b) m where
  genericUnionResolver sym (Inl a) = genericUnionResolver sym a
  genericUnionResolver sym (Inr b) = genericUnionResolver sym b

instance
  ( Applicative m
  , IsSymbol fullName
  , HFoldlWithIndex (ToResolverProps err m) (FieldMap err m) { | arg } (FieldMap err m)
  , Append sym name fullName
  ) =>
  GenericUnionResolver err sym (Constructor name (Argument { | arg })) m where
  genericUnionResolver _sym a = unsafeToObjectResolverRep (Proxy :: Proxy fullName) a
else instance
  ( ToResolver err arg m
  ) =>
  GenericUnionResolver err _sym (Constructor name (Argument arg)) m where
  genericUnionResolver _sym (Constructor (Argument arg)) = toResolver arg