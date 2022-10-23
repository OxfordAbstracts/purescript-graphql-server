module GraphQL.Resolver.ToResolver
  ( FieldMap(..)
  , ToResolverProps(..)
  , class GetArgResolver
  , class ToResolver
  , getArgResolver
  , makeFields
  , objectResolver
  , resolveNode
  , toResolver
  , resolveEnum
  ) where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson)
import Data.Argonaut.Encode.Generic (class EncodeLiteral, encodeLiteralSum)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), from)
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (class IsSymbol, reflectSymbol)
import GraphQL.GqlRep (class GqlRep, GEnum, GScalar)
import GraphQL.Resolver.GqlIo (GqlIo)
import GraphQL.Resolver.JsonResolver (Field, Resolver(..))
import GraphQL.Resolver.JsonResolver as JsonResolver
import GraphQL.Server.GqlError (ResolverError(..))
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Type.Proxy (Proxy(..))

class ToResolver a m | m -> m where
  toResolver :: a -> JsonResolver.Resolver m

unsafeResolveNode :: forall m a. Applicative m => EncodeJson a => a -> Resolver m
unsafeResolveNode a = Node $ pure $ encodeJson a

resolveNode :: forall m a name. GqlRep a GScalar name => Applicative m => EncodeJson a => a -> Resolver m
resolveNode = unsafeResolveNode

unsafeResolveNodeWith
  :: forall a m
   . Applicative m
  => (a -> Json)
  -> a
  -> Resolver m
unsafeResolveNodeWith encode a = Node $ pure $ encode a

resolveEnum
  :: forall m a rep name
   . Applicative m
  => Generic a rep
  => EncodeLiteral rep
  => GqlRep a GEnum name
  => a
  -> Resolver m
resolveEnum = unsafeResolveNodeWith encodeLiteralSum

resolveAsync :: forall m a. Functor m => ToResolver a m => m a -> Resolver m
resolveAsync a = ResolveAsync $ toResolver <$> a

instance (ToResolver a (GqlIo m), Functor m) => ToResolver (GqlIo m a) (GqlIo m) where
  toResolver a = resolveAsync a

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
