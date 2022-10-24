module GraphQL.Server.Schema.Introspection.Types.DirectiveLocation where

import Prelude

import Data.Argonaut (class EncodeJson, encodeJson)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import GraphQL.Server.GqlRep (class GqlRep, GEnum)
import GraphQL.Resolver.ToResolver (class ToResolver, toEnumResolver)

data IDirectiveLocation
  = QUERY
  | MUTATION
  | SUBSCRIPTION
  | FIELD
  | FRAGMENT_DEFINITION
  | FRAGMENT_SPREAD
  | INLINE_FRAGMENT
  | SCHEMA
  | SCALAR
  | OBJECT
  | FIELD_DEFINITION
  | ARGUMENT_DEFINITION
  | INTERFACE
  | UNION
  | ENUM
  | ENUM_VALUE
  | INPUT_OBJECT
  | INPUT_FIELD_DEFINITION

derive instance Generic IDirectiveLocation _

instance Show IDirectiveLocation where
  show a = genericShow a

instance EncodeJson IDirectiveLocation where
  encodeJson = show >>> encodeJson

derive instance Eq IDirectiveLocation

instance GqlRep IDirectiveLocation GEnum "IDirectiveLocation"

instance (Applicative m) => ToResolver IDirectiveLocation m where
  toResolver a = toEnumResolver a