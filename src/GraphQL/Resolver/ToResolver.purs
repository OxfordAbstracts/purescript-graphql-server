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
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Type.Proxy (Proxy(..))

class ToResolver a m where
  toResolver :: a -> JsonResolver.Resolver m
  printGql :: Proxy (m a) -> String

instance Applicative m => ToResolver Boolean m where
  toResolver a = Node $ pure $ encodeJson a
  printGql _ = "Boolean"

instance Applicative m => ToResolver Int m where
  toResolver a = Node $ pure $ encodeJson a
  printGql _ = "Int"

instance Applicative m => ToResolver Number m where
  toResolver a = Node $ pure $ encodeJson a
  printGql _ = "Float"

instance Applicative m => ToResolver String m where
  toResolver a = Node $ pure $ encodeJson a
  printGql _ = "String"

instance Applicative m => ToResolver Json m where
  toResolver a = Node $ pure $ encodeJson a
  printGql _ = "Json"

instance (Applicative m, ToResolver a m) => ToResolver (List a) m where
  toResolver a = ListResolver $ map toResolver a
  printGql _ = "List"

instance (Applicative m, ToResolver a m) => ToResolver (Array a) m where
  toResolver a = ListResolver $ map toResolver $ List.fromFoldable a
  printGql _ = "List"


instance
  ( Applicative m
  , HFoldlWithIndex ToResolverProps (FieldMap m) { | r } (FieldMap m)
  , IsSymbol name
  ) =>
  ToResolver (GqlObj name { | r }) m where
  toResolver (GqlObj a) = Fields
    { fields: makeFields a
    }
  printGql _ = reflectSymbol (Proxy :: Proxy name)

-- instance
--   ( Applicative m
--   , HFoldlWithIndex ToResolverProps (FieldMap m) { | r } (FieldMap m)
--   ) =>
--   ToResolver ({ | r }) m where
--   toResolver a = Fields
--     { fields: makeFields a
--     }
--   printGql _ = "List"

instance
  ( Applicative m
  , Newtype n { | r } 
  , HFoldlWithIndex ToResolverProps (FieldMap m) { | r } (FieldMap m)
  ) =>
  ToResolver (GqlNew n) m where
  toResolver = unwrap >>> unwrap >>> \a -> Fields
    { fields: makeFields a }
  printGql _ = "TODO"

instance (Functor m, EncodeJson a) => ToResolver (GqlIo m a) (GqlIo m) where
  toResolver m = Node $ map encodeJson m
  printGql _ = "TODO"

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