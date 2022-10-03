module GraphQL.Resolver.ToResolver where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson)
import Data.Either (Either(..))
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (class IsSymbol, reflectSymbol)
import GraphQL.Resolver.GqlIo (GqlIo)
import GraphQL.Resolver.JsonResolver (Field, Resolver(..))
import GraphQL.Resolver.JsonResolver as JsonResolver
import GraphQL.Resolver.Resolver.GqlObject (GqlObj(..), GqlNew)
import GraphQL.Server.GqlError (ResolverError(..))
import GraphQL.Server.Schema (GqlRoot(..))
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Type.Proxy (Proxy)

class ToResolver a m where
  toResolver :: a -> JsonResolver.Resolver m

resolveNode :: forall f a. Applicative f => EncodeJson a => a -> Resolver f
resolveNode a = Node $ pure $ encodeJson a

resolveAsync :: forall f a. Applicative f => ToResolver a f => a -> Resolver f
resolveAsync a = ResolveAsync $ pure $ toResolver a

instance (ToResolver a (GqlIo m), Applicative m) => ToResolver (GqlIo m a) (GqlIo m) where
  toResolver = resolveAsync

instance Applicative m => ToResolver Boolean m where
  toResolver = resolveNode

instance Applicative m => ToResolver Int m where
  toResolver = resolveNode

instance Applicative m => ToResolver Number m where
  toResolver = resolveNode

instance Applicative m => ToResolver String m where
  toResolver = resolveNode

instance Applicative m => ToResolver Json m where
  toResolver = resolveNode

instance Applicative m => ToResolver Unit m where
  toResolver = resolveNode

instance Applicative m => ToResolver Void m where
  toResolver = resolveNode

instance (Applicative m, ToResolver a m) => ToResolver (List a) m where
  toResolver a = ListResolver $ map toResolver a

instance (Applicative m, ToResolver a m) => ToResolver (Array a) m where
  toResolver a = ListResolver $ map toResolver $ List.fromFoldable a

instance ToResolver a m => ToResolver (Unit -> a) m where
  toResolver a = toResolver $ a unit

instance
  ( Applicative m
  , HFoldlWithIndex ToResolverProps (FieldMap m) { | r } (FieldMap m)
  ) =>
  ToResolver (GqlObj name { | r }) m where
  toResolver (GqlObj a) = Fields
    { fields: makeFields a
    }

instance
  ( Applicative m
  , HFoldlWithIndex ToResolverProps (FieldMap m) ({ query :: q, mutation :: mut }) (FieldMap m)
  ) =>
  ToResolver (GqlRoot q mut) m where
  toResolver (GqlRoot root) = Fields
    { fields: makeFields root
    }

instance
  ( Applicative m
  , HFoldlWithIndex ToResolverProps (FieldMap m) { | r } (FieldMap m)
  ) =>
  ToResolver ({ | r }) m where
  toResolver a = Fields
    { fields: makeFields a
    }

instance
  ( Applicative m
  , Newtype n { | r }
  , HFoldlWithIndex ToResolverProps (FieldMap m) { | r } (FieldMap m)
  ) =>
  ToResolver (GqlNew n) m where
  toResolver = unwrap >>> unwrap >>> \a -> Fields
    { fields: makeFields a }

makeFields
  :: forall r m
   . HFoldlWithIndex ToResolverProps (FieldMap m) { | r } (FieldMap m)
  => { | r }
  -> Map String (Field m)
makeFields r =
  unwrap $ hfoldlWithIndex ToResolverProps ((FieldMap Map.empty) :: FieldMap m) r

data ToResolverProps = ToResolverProps

newtype FieldMap m = FieldMap (Map String (Field m))

derive instance Newtype (FieldMap m) _

instance
  ( IsSymbol sym
  , GetArgResolver a m
  ) =>
  FoldingWithIndex ToResolverProps (Proxy sym) (FieldMap m) a (FieldMap m) where
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

instance ToResolver a m => GetArgResolver (Unit -> a) m where
  getArgResolver a = \_ -> toResolver (a unit)

else instance (DecodeJson a, ToResolver b m) => GetArgResolver (a -> b) m where
  getArgResolver fn { args } = case decodeJson args of
    Left err -> FailedResolver $ ResolverDecodeError err
    Right a -> toResolver $ fn a

else instance ToResolver a m => GetArgResolver a m where
  getArgResolver a = \_ -> toResolver a