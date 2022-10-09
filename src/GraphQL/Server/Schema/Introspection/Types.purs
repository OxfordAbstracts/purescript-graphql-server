module GraphQL.Server.Schema.Introspection.Types where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Maybe (Maybe)
import GraphQL.Resolver.GqlIo (GqlIo)
import GraphQL.Resolver.ToResolver (class ToResolver, genericResolver)
import Unsafe.Coerce (unsafeCoerce)

newtype ISchema = ISchema
  { types :: List IType
  , queryType :: IType
  , mutationType :: Maybe IType
  , subscriptionType :: Maybe IType
  , directives :: List IDirective
  }

derive instance Generic ISchema _
instance Applicative m => ToResolver ISchema (GqlIo m) where
  toResolver a = genericResolver a

newtype IType = IType
  { kind :: ITypeKind
  , name :: Maybe String
  , description :: Maybe String
  , fields :: { includeDeprecated :: Maybe Boolean } -> Maybe (List IField)
  , interfaces :: Maybe (List IType)
  , possibleTypes :: Maybe (List IType)
  , enumValues :: { includeDeprecated :: Maybe Boolean } -> Maybe (List IEnumValue)
  , inputFields :: Maybe (List IInputValue)
  , ofType :: Maybe IType
  }

derive instance Generic IType _

instance Applicative m => ToResolver IType m where
  toResolver a = genericResolver a

data ITypeKind
  = SCALAR
  | OBJECT
  | INTERFACE
  | UNION
  | ENUM
  | INPUT_OBJECT
  | LIST
  | NON_NULL

derive instance Generic ITypeKind _
instance Applicative m => ToResolver ITypeKind m where
  toResolver a = unsafeCoerce a

newtype IField = IField
  { name :: String
  , description :: Maybe String
  , args :: List IInputValue
  , type :: IType
  , isDeprecated :: Boolean
  , deprecationReason :: Maybe String
  }

derive instance Generic IField _
instance Applicative m => ToResolver IField m where
  toResolver a = genericResolver a

newtype IInputValue = IInputValue
  { name :: String
  , description :: Maybe String
  , type :: IType
  , defaultValue :: Maybe String
  }

derive instance Generic IInputValue _
instance Applicative m => ToResolver IInputValue m where
  toResolver a = genericResolver a

newtype IEnumValue = IEnumValue
  { name :: String
  , description :: Maybe String
  , isDeprecated :: Boolean
  , deprecationReason :: Maybe String
  }

derive instance Generic IEnumValue _
instance Applicative m => ToResolver IEnumValue m where
  toResolver a = genericResolver a

newtype IDirective = IDirective
  { name :: String
  , description :: Maybe String
  , locations :: List IDirectiveLocation
  , args :: List IInputValue
  }

derive instance Generic IDirective _
instance Applicative m => ToResolver IDirective m where
  toResolver a = genericResolver a

data IDirectiveLocation
  = DirectiveLocation_QUERY
  | DirectiveLocation_MUTATION
  | DirectiveLocation_SUBSCRIPTION
  | DirectiveLocation_FIELD
  | DirectiveLocation_FRAGMENT_DEFINITION
  | DirectiveLocation_FRAGMENT_SPREAD
  | DirectiveLocation_INLINE_FRAGMENT
  | DirectiveLocation_SCHEMA
  | DirectiveLocation_SCALAR
  | DirectiveLocation_OBJECT
  | DirectiveLocation_FIELD_DEFINITION
  | DirectiveLocation_ARGUMENT_DEFINITION
  | DirectiveLocation_INTERFACE
  | DirectiveLocation_UNION
  | DirectiveLocation_ENUM
  | DirectiveLocation_ENUM_VALUE
  | DirectiveLocation_INPUT_OBJECT
  | DirectiveLocation_INPUT_FIELD_DEFINITION

derive instance Generic IDirectiveLocation _
instance Applicative m => ToResolver IDirectiveLocation m where
  toResolver a = unsafeCoerce a
