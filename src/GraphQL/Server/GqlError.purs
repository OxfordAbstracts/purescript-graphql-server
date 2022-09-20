module GraphQL.Server.GqlError where

import Prelude

import Data.Argonaut (JsonDecodeError)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Parsing (ParseError)

data GqlError
  = CouldNotParseRequest ParseError
  | NoOperationDefinition
  | OtherError String
  | ResolverError ResolverError

derive instance Eq GqlError
derive instance Generic GqlError _
instance Show GqlError where
  show = genericShow

data ResolverError
  = SelectionSetAtNodeValue
  | MissingSelectionSet
  | NoFields
  | FieldNotFound
  | OtherFailure
  | ResolverDecodeError JsonDecodeError

derive instance Eq ResolverError
derive instance Generic ResolverError _
instance Show ResolverError where
  show = genericShow

