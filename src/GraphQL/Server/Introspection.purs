module GraphQL.Server.Introspection where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Map (lookup)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import GraphQL.Server.Introspection.Types (ISchema(..), IType(..))
import GraphQL.Server.Gql (class Gql, object)

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

instance Gql Introspection where 
  gql = object
