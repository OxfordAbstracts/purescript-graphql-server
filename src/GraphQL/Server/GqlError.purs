module GraphQL.Server.GqlError where

import Prelude

import Data.Argonaut (JsonDecodeError)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Parsing (ParseError)

data GqlError
  = ParseGqlDocumentError ParseError
  | ParseGqlRequestError JsonDecodeError
  | NoOperationDefinition
  | NoOperationDefinitionWithGivenName String
  | MultipleOperationDefinitions
  | VariableInputError VariableInputError
  | SubscriptionsNotSupported
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
  | MaximumDepthExceeded
  | NoMutationRoot
  | ResolverDecodeError JsonDecodeError

derive instance Eq ResolverError
derive instance Generic ResolverError _
instance Show ResolverError where
  show = genericShow

data VariableInputError
  = VariableNotProvided String
  | VariableIsNotInputType String
  | VariableTypeNotFound String
  | VariableHasNoValue String

derive instance Eq VariableInputError
derive instance Generic VariableInputError _
instance Show VariableInputError where
  show = genericShow

