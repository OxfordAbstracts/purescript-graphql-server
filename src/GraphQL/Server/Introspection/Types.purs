module GraphQL.Server.Introspection.Types where

import Prelude

import Data.Argonaut (class EncodeJson, encodeJson)
import Data.Enum (class Enum)
import Data.Enum.Generic (genericPred, genericSucc)
import Data.Generic.Rep (class Generic)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Effect.Aff (Aff)
import GraphQL.Server.Introspection.Types.DirectiveLocation (IDirectiveLocation)
import Safe.Coerce (coerce)

newtype ISchema = ISchema
  { types :: List IType
  , queryType :: IType
  , mutationType :: Maybe IType
  , subscriptionType :: Maybe IType
  , directives :: List IDirective
  }

derive instance Generic ISchema _

derive instance Newtype ISchema _

newtype IType = IType IType_T

type IType_T =
  { kind :: ITypeKind
  , name :: Maybe String
  , description :: Maybe String
  , fields :: { includeDeprecated :: Maybe Boolean } -> Aff (Maybe (List IField))
  , interfaces :: Maybe (List IType)
  , possibleTypes :: Maybe (List IType)
  , enumValues :: { includeDeprecated :: Maybe Boolean } -> Maybe (List IEnumValue)
  , inputFields :: Maybe (List IInputValue)
  , ofType :: Maybe IType
  }
  

defaultIType :: IType_T
defaultIType =
  { kind: SCALAR
  , name: Nothing
  , description: Nothing
  , fields: \_ -> pure Nothing
  , interfaces: Nothing
  , possibleTypes: Nothing
  , enumValues: const Nothing
  , inputFields: Nothing
  , ofType: Nothing
  }
modifyIType :: (IType_T -> IType_T) -> IType -> IType
modifyIType = coerce

scalarType :: String -> IType
scalarType name = IType defaultIType { name = Just name }

unnamed :: ITypeKind -> IType
unnamed kind = IType defaultIType { kind = kind }

derive instance Generic IType _

derive instance Newtype IType _

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

derive instance Eq ITypeKind

derive instance Ord ITypeKind

instance Show ITypeKind where
  show = genericShow

instance EncodeJson ITypeKind where
  encodeJson = show >>> encodeJson

instance Enum ITypeKind where
  succ = genericSucc
  pred = genericPred

newtype IField = IField IField_T

type IField_T =
  { name :: String
  , description :: Maybe String
  , args :: List IInputValue
  , type :: IType
  , isDeprecated :: Boolean
  , deprecationReason :: Maybe String
  }

defaultIField :: IField_T
defaultIField =
  { name: ""
  , description: Nothing
  , args: Nil
  , type: IType defaultIType
  , isDeprecated: false
  , deprecationReason: Nothing
  }

derive instance Generic IField _
derive instance Newtype IField _

newtype IInputValue = IInputValue
  { name :: String
  , description :: Maybe String
  , type :: IType
  , defaultValue :: Maybe String
  }

derive instance Generic IInputValue _

derive instance Newtype IInputValue _

newtype IEnumValue = IEnumValue
  { name :: String
  , description :: Maybe String
  , isDeprecated :: Boolean
  , deprecationReason :: Maybe String
  }

derive instance Generic IEnumValue _

instance Show IEnumValue where
  show = genericShow

newtype IDirective = IDirective
  { name :: String
  , description :: Maybe String
  , locations :: List IDirectiveLocation
  , args :: List IInputValue
  }

derive instance Generic IDirective _

