module GraphQL.Server.Schema.Introspection.GetType where

import Prelude

import Data.Generic.Rep (class Generic, Argument, Constructor)
import Data.List (List(..), reverse, (:))
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import GraphQL.Record.Unsequence (class UnsequenceProxies, unsequenceProxies)
import GraphQL.Resolver.GqlIo (GqlIo)
import GraphQL.Server.Schema.Introspection.GetInputValue (getIInputValues)
import GraphQL.Server.Schema.Introspection.GqlNullable (class GqlNullable, isNullable)
import GraphQL.Server.Schema.Introspection.Types (IField(..), IType(..), IType_T, defaultIType)
import GraphQL.Server.Schema.Introspection.Types as IT
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Safe.Coerce (coerce)
import Type.Proxy (Proxy(..))

-- class GetIType :: forall k.  k -> Constraint
class GetIType :: forall k. k -> Constraint
class GqlNullable a <= GetIType a where
  getITypeImpl :: Proxy a -> IType

getIType :: forall a. GetIType a => Proxy a -> IType
getIType a = getITypeWithNullable a

nodeITypes :: forall a. GetIType a => Proxy a -> List IType
nodeITypes a = pure $ getITypeWithNullable a

getITypeWithNullable :: forall a. GetIType a => Proxy a -> IType
getITypeWithNullable a =
  if isNullable a then
    t
  else
    unnamed IT.NON_NULL # modifyIType _ { ofType = Just t }
  where
  t = getITypeImpl a

instance GetIType Boolean where
  getITypeImpl = scalar "Boolean"

instance GetIType Int where
  getITypeImpl = scalar "Int"

instance GetIType Number where
  getITypeImpl = scalar "Float"

instance GetIType String where
  getITypeImpl = scalar "String"

instance (GetIType a) => GetIType (Array a) where
  getITypeImpl _ = unnamed IT.LIST # modifyIType _
    { ofType = Just $ getITypeWithNullable (Proxy :: Proxy a)
    }

instance (GetIType a) => GetIType (List a) where
  getITypeImpl _ = unnamed IT.LIST # modifyIType _ { ofType = Just $ getITypeWithNullable (Proxy :: Proxy a) }

instance (GetIType a) => GetIType (Maybe a) where
  getITypeImpl _ = getITypeImpl (Proxy :: Proxy a)

instance (GetIType b) => GetIType (a -> b) where
  getITypeImpl _ = getITypeImpl (Proxy :: Proxy b)

instance (GetIType a) => GetIType (GqlIo m a) where
  getITypeImpl _ = getITypeImpl (Proxy :: Proxy a)

genericGetIType
  :: forall name r a
   . Generic a (Constructor name (Argument { | r }))
  => GetIFields { | r }
  => IsSymbol name
  => Proxy a
  -> IType
genericGetIType _ =
  IType defaultIType
    { name = Just $ reflectSymbol (Proxy :: Proxy name)
    , kind = IT.OBJECT
    , fields = \_ -> Just $ getIFields (Proxy :: Proxy { | r })
    }

class GetIFields :: forall k. k -> Constraint
class GetIFields a where
  getIFields :: Proxy a -> List IField

instance
  ( HFoldlWithIndex (GetIFieldsProps) (List IField) { | p } (List IField)
  , UnsequenceProxies { | r } { | p }
  ) =>
  GetIFields { | r } where
  getIFields r = getRecordIFields ((unsequenceProxies r) :: { | p })

data GetIFieldsProps = GetIFieldsProps

instance
  ( IsSymbol label
  , GetIType a
  ) =>
  FoldingWithIndex (GetIFieldsProps) (Proxy label) (List IField) (Proxy a) (List IField) where
  foldingWithIndex (GetIFieldsProps) sym defs a = def : defs
    where
    def = IField
      { args: getIInputValues a
      , deprecationReason: Nothing
      , description: Nothing
      , isDeprecated: false
      , name: reflectSymbol sym
      , type: getITypeWithNullable a
      }

getRecordIFields
  :: forall r
   . HFoldlWithIndex (GetIFieldsProps) (List IField) { | r } (List IField)
  => { | r }
  -> (List IField)
getRecordIFields =
  hfoldlWithIndex (GetIFieldsProps :: GetIFieldsProps) (mempty :: List IField) >>> reverse

modifyIType :: (IType_T -> IType_T) -> IType -> IType
modifyIType = coerce

scalar :: forall n. String -> n -> IType
scalar name _ = IType defaultIType { name = Just name }

unnamed :: IT.ITypeKind -> IType
unnamed kind = IType defaultIType { kind = kind }
