module GraphQL.Server.Schema.Introspection where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Map (lookup)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import GraphQL.GqlRep (class GqlRep, GObject)
import GraphQL.Resolver.JsonResolver (Resolver)
import GraphQL.Resolver.ToResolver (class ToResolver, toObjectResolver, toResolver)
import GraphQL.Server.Schema.Introspection.Types (ISchema(..), IType(..))

makeIntrospectionResolver :: forall m. Applicative m => ISchema -> Resolver m
makeIntrospectionResolver = toResolver <<< Introspection <<< getIntrospection

getIntrospection :: ISchema -> Introspection_T
getIntrospection schema@(ISchema { types }) =
  { __schema: schema
  , __type: \{ name } -> lookup (Just name) typeMap
  }
  where
  typeMap = Map.fromFoldable $ types <#> \iType@(IType { name }) -> Tuple name iType

newtype Introspection = Introspection Introspection_T
type Introspection_T = { | (IntrospectionRow ()) }

type IntrospectionRow r =
  ( __schema :: ISchema
  , __type :: { name :: String } -> Maybe IType
  | r
  )

derive instance Generic Introspection _

instance GqlRep Introspection GObject "Introspection"

instance Applicative m => ToResolver Introspection m where
  toResolver a = toObjectResolver a
