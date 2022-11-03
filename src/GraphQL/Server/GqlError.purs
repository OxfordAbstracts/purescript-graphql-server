module GraphQL.Server.GqlError
  ( GqlError(..)
  , FailedToResolve(..)
  , VariableInputError(..)
  ) where

import Prelude

import Data.Argonaut (JsonDecodeError)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Parsing (ParseError)

data GqlError
  = NotAuthorized
  | ParseGqlDocumentError ParseError
  | ParseGqlRequestError JsonDecodeError
  | NoOperationDefinition
  | NoOperationDefinitionWithGivenName String
  | MultipleOperationDefinitions
  | VariableInputError VariableInputError
  | SubscriptionsNotSupported
  | OtherError String

derive instance Eq GqlError
derive instance Generic GqlError _
instance Show GqlError where
  show = genericShow

data FailedToResolve err
  = SelectionSetAtNodeValue
  | MissingSelectionSet
  | NoFields
  | FieldNotFound
  | ResolverError err
  | MaximumDepthExceeded
  | NoMutationRoot
  | ResolverDecodeError JsonDecodeError
  | NotAuthorizedAtNode

derive instance Eq err => Eq (FailedToResolve err)
derive instance Generic (FailedToResolve err) _
derive instance Functor FailedToResolve

instance Show err => Show (FailedToResolve err) where
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

