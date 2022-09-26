module GraphQL.Resolver.ToResolver where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson)
import Data.Either (Either(..))
import Data.List (List(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Effect.Aff (Aff)
import GraphQL.Resolver.GqlIo (GqlIo)
import GraphQL.Resolver.Resolver.ResolveTo (GqlObj(..))
import GraphQL.Resolver.JsonResolver (Field, Resolver(..))
import GraphQL.Resolver.JsonResolver as JsonResolver
import GraphQL.Server.GqlError (ResolverError(..))
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Type.Proxy (Proxy)

class ToJsonResolver a m where
  toJsonResolver :: a -> JsonResolver.Resolver m

instance Applicative m => ToJsonResolver Boolean m where
  toJsonResolver a = Node $ pure $ encodeJson a

instance Applicative m => ToJsonResolver Int m where
  toJsonResolver a = Node $ pure $ encodeJson a

instance Applicative m => ToJsonResolver Number m where
  toJsonResolver a = Node $ pure $ encodeJson a

instance Applicative m => ToJsonResolver String m where
  toJsonResolver a = Node $ pure $ encodeJson a

instance Applicative m => ToJsonResolver Json m where
  toJsonResolver a = Node $ pure $ encodeJson a

instance (EncodeJson a) => ToJsonResolver (Aff a) Aff where
  toJsonResolver m = Node $ map encodeJson m

instance (EncodeJson a, Applicative m) => ToJsonResolver (GqlIo m a) (GqlIo m) where
  toJsonResolver m = Node $ map encodeJson m

instance (Applicative m, HFoldlWithIndex ToJsonResolverProps (FieldMap m) { | r } (FieldMap m)) => ToJsonResolver (GqlObj name { | r }) m where
  toJsonResolver (GqlObj a) = Fields
    { fields: makeFields a
    }

makeFields
  :: forall r m
   . HFoldlWithIndex ToJsonResolverProps (FieldMap m) { | r } (FieldMap m)
  => { | r }
  -> Map String (Field m)
makeFields r =
  unwrap $ hfoldlWithIndex ToJsonResolverProps ((FieldMap Map.empty) :: FieldMap m) r

data ToJsonResolverProps = ToJsonResolverProps

newtype FieldMap m = FieldMap (Map String (Field m))

derive instance Newtype (FieldMap m) _

instance
  ( IsSymbol sym
  , GetArgTypes a
  , GetArgResolver a m
  ) =>
  FoldingWithIndex ToJsonResolverProps (Proxy sym) (FieldMap m) a (FieldMap m) where
  foldingWithIndex ToJsonResolverProps prop (FieldMap fieldMap) a =
    FieldMap $ Map.insert name field fieldMap
    where
    name = reflectSymbol prop

    field :: Field m
    field =
      { name
      , resolver: getArgResolver a
      }

class GetArgTypes a where
  getArgTypes
    :: a
    -> List
         { name :: String
         , required :: Boolean
         , type :: String
         }

instance GetArgTypes a where
  getArgTypes _ = Nil

class GetArgResolver a m where
  getArgResolver
    :: a
    -> { args :: Json }
    -> JsonResolver.Resolver m

instance (DecodeJson a, ToJsonResolver b m) => GetArgResolver (a -> b) m where
  getArgResolver fn { args } = case decodeJson args of
    Left err -> FailedResolver $ ResolverDecodeError err
    Right a -> toJsonResolver $ fn a
else instance ToJsonResolver a m => GetArgResolver a m where
  getArgResolver a _ = toJsonResolver a