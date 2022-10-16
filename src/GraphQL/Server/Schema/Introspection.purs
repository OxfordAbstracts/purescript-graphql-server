module GraphQL.Server.Schema.Introspection where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Map (lookup)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import GraphQL.Resolver.JsonResolver (Resolver)
import GraphQL.Resolver.ToResolver (class ToResolver, objectResolver, toResolver)
import GraphQL.Server.Schema.Introspection.Types (ISchema(..), IType(..))

makeIntrospectionResolver :: forall m. Applicative m => ISchema -> Resolver m
makeIntrospectionResolver schema@(ISchema { types }) = toResolver introspection
  where
  introspection = Introspection
    { __schema: schema
    , __type: \{ name } -> lookup (Just name) typeMap
    }

  typeMap = Map.fromFoldable $ types <#> \iType@(IType { name }) -> Tuple name iType

newtype Introspection = Introspection
  { __schema :: ISchema
  , __type :: { name :: String } -> Maybe IType
  }

derive instance Generic Introspection _

instance Applicative m => ToResolver Introspection m where
  toResolver a = objectResolver a
