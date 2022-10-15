module GraphQL.Server.Schema.Introspection.GetType where

import Prelude

import Data.Generic.Rep (class Generic, Argument, Constructor, from)
import Data.List (List(..), reverse, (:))
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import GraphQL.Record.Unsequence (class UnsequenceProxies, unsequenceProxies)
import GraphQL.Server.Schema.Introspection.Types (IField(..), IType(..), IType_T, defaultIType)
import GraphQL.Server.Schema.Introspection.Types as IT
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Safe.Coerce (coerce)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)




class GetIType :: forall k. k -> Constraint
class GetIType a where
  getITypeImpl :: Proxy a -> IType
  gqlNullable :: Proxy a -> Boolean
  getITypes :: Proxy a -> List IType


nodeITypes :: forall a. GetIType a => Proxy a -> List IType
nodeITypes a = pure $ getIType a

getIType :: forall a. GetIType a => Proxy a -> IType
getIType a =
  if gqlNullable a then
    t
  else
    unnamed IT.NON_NULL # modifyIType _ { ofType = Just t }
  where
  t = getITypeImpl a

instance GetIType Boolean where
  getITypeImpl _ = scalar "Boolean"
  gqlNullable _ = false
  getITypes a = nodeITypes a

else instance GetIType Int where
  getITypeImpl _ = scalar "Int"
  gqlNullable _ = false
  getITypes a = nodeITypes a

else instance GetIType Number where
  getITypeImpl _ = scalar "Float"
  gqlNullable _ = false
  getITypes a = nodeITypes a

else instance GetIType String where
  getITypeImpl _ = scalar "String"
  gqlNullable _ = false
  getITypes a = nodeITypes a

else instance GetIType a => GetIType (Array a) where
  getITypeImpl _ = unnamed IT.LIST # modifyIType _ { ofType = Just $ getIType (Proxy :: Proxy a) }
  gqlNullable _ = false
  getITypes a = nodeITypes a

else instance GetIType a => GetIType (List a) where
  getITypeImpl _ = unnamed IT.LIST # modifyIType _ { ofType = Just $ getIType (Proxy :: Proxy a) }
  gqlNullable _ = false
  getITypes a = nodeITypes a

else instance GetIType a => GetIType (Maybe a) where
  getITypeImpl _ = getITypeImpl (Proxy :: Proxy a)
  gqlNullable _ = true
  getITypes a = nodeITypes a

else instance (Generic a rep, GenericGetIType rep) => GetIType a where
  getITypeImpl a = genericGetITypeImpl $ map from a
  gqlNullable a = genericGqlNullable $ map from a
  getITypes a = genericGetITypes $ map from a


class GenericGetIType :: forall k. k -> Constraint
class GenericGetIType a where
  genericGetITypeImpl :: Proxy a -> IType
  genericGqlNullable :: Proxy a -> Boolean
  genericGetITypes :: Proxy a -> List IType


instance (IsSymbol name, GetIFields { |r }) => GenericGetIType (Constructor name (Argument {|r})) where 
  genericGetITypeImpl _ = 
    IType defaultIType 
      { name = Just $ reflectSymbol (Proxy :: Proxy name)
      , kind = IT.OBJECT
      , fields = \_ -> Just $ getIFields (Proxy :: Proxy {|r})
      }
  genericGqlNullable _ = false
  genericGetITypes _ = Nil


class GetIFields :: forall k. k -> Constraint
class GetIFields  a where
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
  -- , GetIType a
  -- , ToGqlType a
  -- , GetArgumentsDefinitionFromFn a
  ) =>
  FoldingWithIndex GetIFieldsProps (Proxy label) (List IField) (Proxy a) (List IField) where
  foldingWithIndex (GetIFieldsProps) sym (defs) a =  def : defs
    where
    def = IField 
      { args: Nil -- :: List IInputValue
      , deprecationReason: Nothing
      , description: Nothing
      , isDeprecated: false
      , name: reflectSymbol sym
      , type: IType defaultIType
      }


getRecordIFields
  :: forall r
   . HFoldlWithIndex GetIFieldsProps (List IField) { | r } (List IField)
  => { | r }
  -> (List IField)
getRecordIFields =
  hfoldlWithIndex GetIFieldsProps (mempty :: List IField) >>> reverse




modifyIType :: (IType_T -> IType_T) -> IType -> IType
modifyIType = coerce

scalar :: String -> IType
scalar name = IType defaultIType { name = Just name }

unnamed :: IT.ITypeKind -> IType
unnamed kind = IType defaultIType { kind = kind }

genericGetRecordITypes :: forall a name r. 
  Generic a (Constructor name (Argument {|r})) => 
  Proxy a -> 
  List IType
genericGetRecordITypes proxy = Nil