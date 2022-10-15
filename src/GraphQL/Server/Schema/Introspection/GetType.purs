module GraphQL.Server.Schema.Introspection.GetType where

import Prelude

import Data.Generic.Rep (class Generic, Argument, Constructor, from)
import Data.List (List(..), reverse, (:))
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Typelevel.Num (class Nat, class Succ, D0, toInt')
import GraphQL.Record.Unsequence (class UnsequenceProxies, unsequenceProxies)
import GraphQL.Server.MaxDepth (MaxDepth, maxDepth)
import GraphQL.Server.Schema.Introspection.GetEnumValues (enumType)
import GraphQL.Server.Schema.Introspection.Types (IField(..), IType(..), ITypeKind, IType_T, defaultIType)
import GraphQL.Server.Schema.Introspection.Types as IT
import GraphQL.Server.Schema.Introspection.Types.DirectiveLocation (IDirectiveLocation)
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Safe.Coerce (coerce)
import Type.Proxy (Proxy(..))

-- class GetIType :: forall k.  k -> Constraint
class GetIType :: forall k. Type -> k -> Constraint
class Nat n <= GetIType n a where
  getITypeImpl :: Proxy a -> Proxy n -> IType
  gqlNullable :: Proxy a -> Proxy n -> Boolean

getIType :: forall a. GetIType MaxDepth a => Proxy a -> IType
getIType a = getITypeWithNullable a (Proxy :: Proxy MaxDepth)

nodeITypes :: forall a n. GetIType n a => Proxy a -> Proxy n -> List IType
nodeITypes a n = pure $ getITypeWithNullable a n

getITypeWithNullable :: forall n a. GetIType n a => Proxy a -> Proxy n -> IType
getITypeWithNullable a n =
  if gqlNullable a n then
    t
  else
    unnamed IT.NON_NULL # modifyIType _ { ofType = Just t }
  where
  t = getITypeImpl a n

instance Nat n => GetIType n Boolean where
  getITypeImpl _ = scalar "Boolean"
  gqlNullable _ _ = false

else instance Nat n => GetIType n Int where
  getITypeImpl _ = scalar "Int"
  gqlNullable _ _ = false

else instance Nat n => GetIType n Number where
  getITypeImpl _ = scalar "Float"
  gqlNullable _ _ = false

else instance Nat n => GetIType n String where
  getITypeImpl _ = scalar "String"
  gqlNullable _ _ = false

else instance (Nat n, GetIType n a) => GetIType n (Array a) where
  getITypeImpl _ _ = unnamed IT.LIST # modifyIType _
    { ofType = Just $ getITypeWithNullable (Proxy :: Proxy a) (Proxy :: Proxy n)
    }
  gqlNullable _ _ = false

else instance (Nat n, GetIType n a) => GetIType n (List a) where
  getITypeImpl _ _ = unnamed IT.LIST # modifyIType _ { ofType = Just $ getITypeWithNullable (Proxy :: Proxy a) (Proxy :: Proxy n) }
  gqlNullable _ _ = false

else instance (Nat n, GetIType n a) => GetIType n (Maybe a) where
  getITypeImpl _ = getITypeImpl (Proxy :: Proxy a)
  gqlNullable _ _ = true

else instance (Nat n) => GetIType n IType where
  getITypeImpl a n =
    IType defaultIType
      { name = Just "Type"
      , kind = IT.OBJECT
      , fields = \_ -> Nothing -- Just $ getIFields (Proxy :: Proxy { | r }) (Proxy :: Proxy n)
      }

  gqlNullable _ _ = false

else instance (Nat n, Generic a rep, CustomGetIType n rep) => GetIType n a where
  getITypeImpl a = customGetIType $ map from a
  gqlNullable _ _ = false

class CustomGetIType :: forall k. Type -> k -> Constraint
class Nat n <= CustomGetIType n a where
  customGetIType :: Proxy a -> Proxy n -> IType

instance (IsSymbol name, Nat n, GetIFields n { | r }) => CustomGetIType n (Constructor name (Argument { | r })) where
  customGetIType _ _ =
    IType defaultIType
      { name = Just $ reflectSymbol (Proxy :: Proxy name)
      , kind = IT.OBJECT
      , fields = \_ -> Just $ getIFields (Proxy :: Proxy { | r }) (Proxy :: Proxy n)
      }

instance Nat n => CustomGetIType n ITypeKind where
  customGetIType kind _n = enumType "Kind" $ map from kind

instance Nat n => CustomGetIType n IDirectiveLocation where
  customGetIType kind _n = enumType "DirectiveLocation" $ map from kind

class GetIFields :: forall k. Type -> k -> Constraint
class Nat n <= GetIFields n a where
  getIFields :: Proxy a -> Proxy n -> List IField

instance
  ( HFoldlWithIndex (GetIFieldsProps n) (List IField) { | p } (List IField)
  , UnsequenceProxies { | r } { | p }
  , Nat n
  ) =>
  GetIFields n { | r } where
  getIFields r _ = getRecordIFields (Proxy :: Proxy n) ((unsequenceProxies r) :: { | p })

data GetIFieldsProps :: forall k. k -> Type
data GetIFieldsProps n = GetIFieldsProps

instance
  ( IsSymbol label
  ) =>
  FoldingWithIndex (GetIFieldsProps D0) (Proxy label) (List IField) (Proxy a) (List IField) where
  foldingWithIndex (GetIFieldsProps) sym (defs) _ = def : defs
    where
    def = IField
      { args: Nil
      , deprecationReason: Nothing
      , description: Nothing
      , isDeprecated: false
      , name: reflectSymbol sym
      , type: IType defaultIType
          { name = Just "Depth limit exceeded"
          , description = Just $
              "You have exceeded the maximum introspection depth of "
                <> show (toInt' maxDepth)
          }
      }
else instance
  ( IsSymbol label
  , GetIType pred a
  , Succ pred n
  ) =>
  FoldingWithIndex (GetIFieldsProps n) (Proxy label) (List IField) (Proxy a) (List IField) where
  foldingWithIndex (GetIFieldsProps) sym (defs) a = def : defs
    where
    def = IField
      { args: Nil -- :: List IInputValue
      , deprecationReason: Nothing
      , description: Nothing
      , isDeprecated: false
      , name: reflectSymbol sym
      , type: getITypeWithNullable a (Proxy :: Proxy pred)
      }

getRecordIFields
  :: forall r n
   . HFoldlWithIndex (GetIFieldsProps n) (List IField) { | r } (List IField)
  => Proxy n
  -> { | r }
  -> (List IField)
getRecordIFields _ =
  hfoldlWithIndex (GetIFieldsProps :: GetIFieldsProps n) (mempty :: List IField) >>> reverse

modifyIType :: (IType_T -> IType_T) -> IType -> IType
modifyIType = coerce

scalar :: forall n. String -> n -> IType
scalar name _ = IType defaultIType { name = Just name }

unnamed :: IT.ITypeKind -> IType
unnamed kind = IType defaultIType { kind = kind }
