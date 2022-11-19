module GraphQL.Server.Schema.Introspection.GetType
  ( GetIFieldsProps(..)
  , GetIInputValuesProps(..)
  , class GetIFields
  , class GetIInputValues
  , class GetGqlType
  , class GetUnionPossibleTypes
  , getObjectType
  , getEnumType
  , getScalarType
  , getUnionType
  , getIFields
  , getIInputValues
  , getType
  , getTypeWithNull
  , getUnionPossibleTypes
  ) where

import Prelude

import Data.Date (Date)
import Data.DateTime (DateTime)
import Data.Generic.Rep (class Generic, Argument, Constructor, Sum)
import Data.List (List(..), reverse, (:))
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Time (Time)
import GraphQL.Record.Unsequence (class UnsequenceProxies, unsequenceProxies)
import GraphQL.Resolver.GqlIo (GqlIo)
import GraphQL.Server.GqlRep (class GqlRep, GEnum, GObject, GUnion)
import GraphQL.Server.Schema.Introspection.GetEnumValues (class GetEnumValues, getEnumValues)
import GraphQL.Server.Schema.Introspection.GqlNullable (class GqlNullable, isNullable)
import GraphQL.Server.Schema.Introspection.Types (IEnumValue(..), IField(..), IInputValue(..), IType(..), ITypeKind(..), IType_T, defaultIType)
import GraphQL.Server.Schema.Introspection.Types as IT
import GraphQL.Server.Schema.RecordTypename (class RecordTypename)
import GraphQL.Server.Schema.Scalar (class Scalar)
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Prim.Symbol (class Append)
import Safe.Coerce (coerce)
import Type.Proxy (Proxy(..))

class GetGqlType :: forall k. k -> Constraint
class GqlNullable a <= GetGqlType a where
  getType :: Proxy a -> IType

getTypeWithNull :: forall a. GetGqlType a => Proxy a -> IType
getTypeWithNull a =
  if isNullable a then
    t
  else
    unnamed IT.NON_NULL # modifyIType _ { ofType = Just t }
  where
  t = getType a

instance GetGqlType Boolean where
  getType = unsafeScalar "Boolean"

instance GetGqlType Int where
  getType = unsafeScalar "Int"

instance GetGqlType Number where
  getType = unsafeScalar "Float"

instance GetGqlType String where
  getType = unsafeScalar "String"

instance GetGqlType Date where
  getType = unsafeScalar "Date"

instance GetGqlType Time where
  getType = unsafeScalar "Time"

instance GetGqlType DateTime where
  getType = unsafeScalar "DateTime"

instance (GetGqlType a) => GetGqlType (Array a) where
  getType _ = unnamed IT.LIST # modifyIType _
    { ofType = Just $ getTypeWithNull (Proxy :: Proxy a)
    }

instance (GetGqlType a) => GetGqlType (List a) where
  getType _ = unnamed IT.LIST # modifyIType _ { ofType = Just $ getTypeWithNull (Proxy :: Proxy a) }

instance (GetGqlType a) => GetGqlType (Maybe a) where
  getType _ = getType (Proxy :: Proxy a)

instance (GetGqlType b) => GetGqlType (a -> b) where
  getType _ = getType (Proxy :: Proxy b)

instance (GetGqlType a) => GetGqlType (GqlIo m a) where
  getType _ = getType (Proxy :: Proxy a)

instance (IsSymbol a) => GetGqlType (Proxy a) where
  getType = unsafeScalar "String"

instance
  ( GetIFields { | r }
  , RecordTypename { | r } name
  , IsSymbol name
  ) =>
  GetGqlType { | r } where
  getType _ = unsafeGetObjectType (Proxy :: Proxy name) (Proxy :: Proxy { | r })

getObjectType
  :: forall name r a
   . Generic a (Constructor name (Argument { | r }))
  => GetIFields { | r }
  => IsSymbol name
  => Proxy a
  -> IType
getObjectType _ = unsafeGetObjectType (Proxy :: Proxy name) (Proxy :: Proxy { | r })

unsafeGetObjectType
  :: forall name r
   . GetIFields { | r }
  => IsSymbol name
  => Proxy name
  -> Proxy { | r }
  -> IType
unsafeGetObjectType _ _ =
  IType defaultIType
    { name = Just $ reflectSymbol (Proxy :: Proxy name)
    , kind = IT.OBJECT
    , fields = \_ -> Just $ getIFields (Proxy :: Proxy { | r })
    }

getEnumType
  :: forall a rep name
   . IsSymbol name
  => Generic a rep
  => GqlRep a GEnum name
  => GetEnumValues rep
  => Proxy a
  -> IType
getEnumType _ = IType defaultIType
  { kind = ENUM
  , name = Just $ reflectSymbol (Proxy :: Proxy name)
  , enumValues = \_ -> Just $ getEnumValues (Proxy :: Proxy rep) <#>
      IEnumValue <<<
        { name: _
        , description: Nothing
        , isDeprecated: false
        , deprecationReason: Nothing
        }
  }

getUnionType
  :: forall a rep name
   . IsSymbol name
  => Generic a rep
  => GqlRep a GUnion name
  => GetUnionPossibleTypes name rep
  => Proxy a
  -> IType
getUnionType _ = IType defaultIType
  { kind = UNION
  , name = Just $ reflectSymbol (Proxy :: Proxy name)
  , possibleTypes = Just $ getUnionPossibleTypes (Proxy :: Proxy name) (Proxy :: Proxy rep)
  }

getScalarType :: forall a name. Scalar a name => IsSymbol name => Proxy a -> IType
getScalarType a = unsafeScalar (reflectSymbol (Proxy :: Proxy name)) a

class GetUnionPossibleTypes :: forall k. Symbol -> k -> Constraint
class GetUnionPossibleTypes sym a where
  getUnionPossibleTypes :: Proxy sym -> Proxy a -> List IType

instance
  ( Append typeName name fullName
  , IsSymbol fullName
  , GetIFields { | r }
  ) =>
  GetUnionPossibleTypes typeName (Constructor name (Argument { | r })) where
  getUnionPossibleTypes _ _ = pure $ IType defaultIType
    { name = Just $ reflectSymbol (Proxy :: Proxy fullName)
    , kind = IT.OBJECT
    , fields = \_ -> Just $ getIFields (Proxy :: Proxy { | r })
    }

else instance
  ( GetGqlType t
  , GqlRep t GObject name
  ) =>
  GetUnionPossibleTypes sym (Constructor ctrName (Argument t)) where
  getUnionPossibleTypes _ _ = pure $ getType (Proxy :: Proxy t)

instance (GetUnionPossibleTypes sym a, GetUnionPossibleTypes sym b) => GetUnionPossibleTypes sym (Sum a b) where
  getUnionPossibleTypes sym _ = getUnionPossibleTypes sym (Proxy :: Proxy a) <> getUnionPossibleTypes sym (Proxy :: Proxy b)

class GetIFields :: forall k. k -> Constraint
class GetIFields a where
  getIFields :: Proxy a -> List IField

instance
  ( HFoldlWithIndex GetIFieldsProps (List IField) { | p } (List IField)
  , UnsequenceProxies { | r } { | p }
  ) =>
  GetIFields { | r } where
  getIFields r = getRecordIFields ((unsequenceProxies r) :: { | p })

data GetIFieldsProps = GetIFieldsProps

instance
  ( IsSymbol label
  , GetGqlType a
  , GetIInputValues a
  ) =>
  FoldingWithIndex GetIFieldsProps (Proxy label) (List IField) (Proxy a) (List IField) where
  foldingWithIndex GetIFieldsProps sym defs a = def : defs
    where
    def = IField
      { args: getIInputValues a
      , deprecationReason: Nothing
      , description: Nothing
      , isDeprecated: false
      , name: reflectSymbol sym
      , type: getTypeWithNull a
      }

getRecordIFields
  :: forall r
   . HFoldlWithIndex GetIFieldsProps (List IField) { | r } (List IField)
  => { | r }
  -> (List IField)
getRecordIFields =
  hfoldlWithIndex (GetIFieldsProps :: GetIFieldsProps) (mempty :: List IField) >>> reverse

class GetIInputValues :: forall k. k -> Constraint
class GetIInputValues a where
  getIInputValues :: Proxy a -> List IInputValue

instance
  ( HFoldlWithIndex (GetIInputValuesProps) (List IInputValue) { | p } (List IInputValue)
  , UnsequenceProxies { | r } { | p }
  ) =>
  GetIInputValues ({ | r } -> a) where
  getIInputValues _r = getRecordIInputValues ((unsequenceProxies (Proxy :: Proxy { | r })) :: { | p })
else instance
  GetIInputValues a where
  getIInputValues _r = Nil

data GetIInputValuesProps = GetIInputValuesProps

instance
  ( IsSymbol label
  , GetGqlType a
  ) =>
  FoldingWithIndex (GetIInputValuesProps) (Proxy label) (List IInputValue) (Proxy (Maybe a)) (List IInputValue) where
  foldingWithIndex (GetIInputValuesProps) sym defs _a = def : defs
    where
    def = IInputValue
      { name: reflectSymbol sym
      , description: Nothing
      , type: getTypeWithNull (Proxy :: Proxy (Maybe a))
      , defaultValue: Nothing
      }
else instance
  ( IsSymbol label
  , GetGqlType a
  ) =>
  FoldingWithIndex (GetIInputValuesProps) (Proxy label) (List IInputValue) (Proxy a) (List IInputValue) where
  foldingWithIndex (GetIInputValuesProps) sym defs a = def : defs
    where
    def = IInputValue
      { name: reflectSymbol sym
      , description: Nothing
      , type: getTypeWithNull a
      , defaultValue: Nothing
      }

getRecordIInputValues
  :: forall r
   . HFoldlWithIndex GetIInputValuesProps (List IInputValue) { | r } (List IInputValue)
  => { | r }
  -> (List IInputValue)
getRecordIInputValues =
  hfoldlWithIndex (GetIInputValuesProps :: GetIInputValuesProps) (mempty :: List IInputValue) >>> reverse

modifyIType :: (IType_T -> IType_T) -> IType -> IType
modifyIType = coerce

unsafeScalar :: forall n. String -> n -> IType
unsafeScalar name _ = IType defaultIType { name = Just name }

unnamed :: IT.ITypeKind -> IType
unnamed kind = IType defaultIType { kind = kind }
