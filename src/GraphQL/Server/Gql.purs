module GraphQL.Server.Gql where

import Prelude

import Data.Argonaut (class EncodeJson, Json, encodeJson)
import Data.Argonaut.Encode.Generic (class EncodeLiteral, encodeLiteralSum)
import Data.Date (Date)
import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), Sum(..), from)
import Data.List (List(..), reverse, (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Time (Time)
import GraphQL.Server.Record.Unsequence (class UnsequenceProxies, unsequenceProxies)
import GraphQL.Server.GqlM (GqlM)
import GraphQL.Server.Resolver.JsonResolver (Field, Resolver(..))
import GraphQL.Server.DateTime (encodeDate, encodeDateTime, encodeTime)
import GraphQL.Server.Decode (class DecodeArg, decodeArg)
import GraphQL.Server.GqlError (FailedToResolve(..))
import GraphQL.Server.Introspection.GqlNullable (class GqlNullable, isNullable)
import GraphQL.Server.Introspection.Types (IDirective, IEnumValue, IField(..), IInputValue(..), ISchema, IType(..), ITypeKind, IType_T, defaultIType)
import GraphQL.Server.Introspection.Types as IT
import GraphQL.Server.Introspection.Types.DirectiveLocation (IDirectiveLocation)
import HTTPure (Request)
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Safe.Coerce (coerce)
import Type.Proxy (Proxy(..))

class GqlNullable a <= Gql a where
  gql :: Unit -> GqlProps a

gql' :: forall a. Gql a => GqlPropsT a
gql' = unwrap $ gql unit

newtype GqlProps a = GqlProps (GqlPropsT a)

derive instance Newtype (GqlProps a) _

type GqlPropsT a =
  { resolver :: a -> Request -> Resolver
  , iType :: Unit -> IType
  }

getIType :: forall a. GqlProps a -> Unit -> IType
getIType (GqlProps { iType }) _ = iType unit

getResolver :: forall a. GqlProps a -> a -> Request -> Resolver
getResolver (GqlProps { resolver }) = resolver

toResolver :: forall a. Gql a => a -> Request -> Resolver
toResolver a req = resolver a req
  where
  (GqlProps { resolver }) = gql unit

instance Gql Void where
  gql _ = jsonScalar "Void"

instance Gql Unit where
  gql _ = jsonScalar "Unit"

instance Gql Boolean where
  gql _ = jsonScalar "Boolean"

instance Gql Int where
  gql _ = jsonScalar "Int"

instance Gql Number where
  gql _ = jsonScalar "Float"

instance Gql String where
  gql _ = jsonScalar "String"

instance IsSymbol sym => Gql (Proxy sym) where
  gql _ = scalar (reflectSymbol >>> encodeJson) "String"

instance Gql Json where
  gql _ = jsonScalar "Json"

instance Gql Date where
  gql _ = scalar encodeDate "Date"

instance Gql Time where
  gql _ = scalar encodeTime "Time"

instance Gql DateTime where
  gql _ = scalar encodeDateTime "DateTime"

instance (GqlNullable a, Gql a) => Gql (Maybe a) where
  gql _ = GqlProps
    { resolver: \a req ->
        maybe Null (flip resolver req) a
    , iType
    }
    where
    GqlProps { resolver, iType } = (gql unit) :: GqlProps a

instance Gql a => Gql (Array a) where
  gql _ = GqlProps
    { resolver: \a req -> ListResolver $ flip resolver req <$> List.fromFoldable a
    , iType: \_ -> unnamed IT.LIST # modifyIType _
        { ofType = Just $ getTypeWithNull (Proxy :: Proxy a)
        }
    }
    where
    GqlProps { resolver } = (gql unit) :: GqlProps a

instance Gql a => Gql (List a) where
  gql _ = GqlProps
    { resolver: \a req -> ListResolver $ flip resolver req <$> a
    , iType: \_ -> unnamed IT.LIST # modifyIType _
        { ofType = Just $ getTypeWithNull (Proxy :: Proxy a)
        }
    }
    where
    GqlProps { resolver } = (gql unit) :: GqlProps a

instance (Gql a) => Gql (GqlM a) where
  gql _ = GqlProps
    { resolver: \a req ->
        AsyncResolver do
          a' <- a
          let
            GqlProps { resolver } = gql unit :: GqlProps a
          pure $ resolver a' req
    , iType: \_ ->
        let
          GqlProps { iType } = gql unit :: GqlProps a
        in
          iType unit
    }

instance Gql ISchema where
  gql = objectWithName "__Schema"

instance Gql IDirective where
  gql = objectWithName "__Directive"

instance Gql IDirectiveLocation where
  gql = enum "__DirectiveLocation"

instance Gql IInputValue where
  gql _ = objectWithName "__InputValue" unit

instance Gql IType where
  gql _ = objectWithName "__Type" unit

instance Gql IField where
  gql = objectWithName "__Field"

instance Gql IEnumValue where
  gql = objectWithName "__EnumValue"

instance Gql ITypeKind where
  gql = enum "__TypeKind"

-- | create an enum graphql type
enum
  :: forall rep a
   . Generic a rep
  => EncodeLiteral rep
  => String
  -> Unit
  -> GqlProps a
enum name _ = GqlProps
  { resolver: \a _ -> Node $ pure $ encodeLiteralSum a
  , iType: \_ -> IType defaultIType
      { name = Just name
      , kind = IT.ENUM
      }
  }

-- | create a union graphql type
union
  :: forall rep a
   . Generic a rep
  => UnionGql rep
  => String
  -> Unit
  -> GqlProps a
union name _ = GqlProps
  { resolver: \a req -> unionResolver name (from a) req
  , iType: \_ -> IType defaultIType
      { name = Just name
      , kind = IT.UNION
      , possibleTypes = Just $ possibleTypes name (Proxy :: Proxy rep)
      }
  }

-- class UnionGql :: forall k. Type -> k -> Constraint
class UnionGql a where
  unionResolver :: String -> a -> Request -> Resolver
  possibleTypes :: String -> Proxy a -> List IType

instance
  ( UnionGql l
  , UnionGql r
  ) =>
  UnionGql (Sum l r) where
  unionResolver name a req = case a of
    Inl l -> unionResolver name l req
    Inr r -> unionResolver name r req
  possibleTypes name _ = possibleTypes name (Proxy :: Proxy l) <> possibleTypes name (Proxy :: Proxy r)

instance
  ( HFoldlWithIndex ToResolverProps FieldMap { | arg } FieldMap
  , GetIFields { | arg }
  , IsSymbol name
  ) =>
  UnionGql (Constructor name (Argument { | arg })) where
  unionResolver ctrName (Constructor (Argument a)) req =
    Fields
      { fields: makeFields typename req a
      , typename
      }
    where
    typename = ctrName <> "_" <> reflectSymbol (Proxy :: Proxy name)
  possibleTypes ctrName _ = pure $ IType defaultIType
    { name = Just typename
    , kind = IT.OBJECT
    , fields = \_ -> Just $ getIFields (Proxy :: Proxy { | arg })
    }
    where
    typename = ctrName <> "_" <> reflectSymbol (Proxy :: Proxy name)
else instance
  ( Gql arg
  ) =>
  UnionGql (Constructor name (Argument arg)) where
  unionResolver _ctrName a req =
    let
      (GqlProps { resolver }) = gql unit :: GqlProps arg
    in
      resolver (coerce a) req
  possibleTypes _ _ =
    let
      (GqlProps { iType }) = gql unit :: GqlProps arg
    in
      pure $ iType unit

-- | create an object graphql type
object
  :: forall r name a
   . Generic a (Constructor name (Argument { | r }))
  => GqlObject a
  => IsSymbol name
  => Unit
  -> GqlProps a
object = objectWithName $ reflectSymbol (Proxy :: Proxy name)

-- | create an object graphql type with a custom name
objectWithName
  :: forall r name a
   . Generic a (Constructor name (Argument { | r }))
  => GqlObject a
  => String
  -> Unit
  -> GqlProps a
objectWithName typename _ = gqlObject typename

class GqlObject a where
  gqlObject :: String -> GqlProps a

instance
  ( Generic a (Constructor name (Argument { | r }))
  , HFoldlWithIndex ToResolverProps FieldMap { | r } FieldMap
  , GetIFields { | r }
  ) =>
  GqlObject a where
  gqlObject typename = makeGqlObject (Proxy :: Proxy { | r }) typename

makeGqlObject
  :: forall arg a name r
   . Generic a (Constructor name (Argument arg))
  => HFoldlWithIndex ToResolverProps FieldMap arg FieldMap
  => GetIFields r
  => Proxy r
  -> String
  -> GqlProps a
makeGqlObject recordProxy typename = GqlProps
  { resolver: \a req ->
      let
        Constructor (Argument r) = from a
      in
        Fields
          { fields: makeFields typename req r
          , typename
          }
  , iType: \_ -> IType defaultIType
      { name = Just typename
      , kind = IT.OBJECT
      , fields = \_ -> Just $ getIFields recordProxy
      }
  }

makeFields
  :: forall a
   . HFoldlWithIndex ToResolverProps FieldMap a FieldMap
  => String
  -> Request
  -> a
  -> Map String (Field)
makeFields typename req r =
  unwrap $ hfoldlWithIndex (ToResolverProps req) resolveTypename r
  where
  resolveTypename = FieldMap $ Map.singleton "__typename"
    { name: "__typename"
    , resolver: \_ -> Node $ pure $ encodeJson typename
    }

data ToResolverProps = ToResolverProps Request

newtype FieldMap = FieldMap (Map String (Field))

derive instance Newtype FieldMap _

instance
  ( IsSymbol sym
  , GetArgResolver a
  ) =>
  FoldingWithIndex (ToResolverProps) (Proxy sym) FieldMap a FieldMap where
  foldingWithIndex (ToResolverProps req) prop (FieldMap fieldMap) a =
    FieldMap $ Map.insert name field fieldMap
    where
    name = reflectSymbol prop

    field :: Field
    field =
      { name
      , resolver: getArgResolver a req
      }

class GetArgResolver a where
  getArgResolver
    :: a
    -> Request
    -> { args :: Json }
    -> Resolver

instance argResolverUnitFn :: Gql a => GetArgResolver (Unit -> a) where
  getArgResolver a req _ = toResolver (a unit) req

else instance argResolverAllFn :: (DecodeArg a, Gql b) => GetArgResolver (a -> b) where
  getArgResolver fn req { args } = case decodeArg args of
    Left err -> FailedResolver $ ResolverDecodeError err
    Right a -> toResolver (fn a) req

else instance argResolverAny :: Gql a => GetArgResolver a where
  getArgResolver a req _ = toResolver a req

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
  , Gql b
  , GetIInputValues (a -> b)
  ) =>
  FoldingWithIndex GetIFieldsProps (Proxy label) (List IField) (Proxy (a -> b)) (List IField) where
  foldingWithIndex GetIFieldsProps sym defs a = def : defs
    where
    def = IField
      { args: getIInputValues a
      , deprecationReason: Nothing
      , description: Nothing
      , isDeprecated: false
      , name: reflectSymbol sym
      , type: getTypeWithNull (Proxy :: Proxy b)
      }
else instance
  ( IsSymbol label
  , Gql a
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
  , Gql a
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

jsonScalar :: forall a. EncodeJson a => String -> GqlProps a
jsonScalar = scalar encodeJson

scalar :: forall a. (a -> Json) -> String -> GqlProps a
scalar encode name = GqlProps
  { resolver: \a _ -> Node $ pure $ encode a
  , iType: \_ -> scalarType name
  }

modifyIType :: (IType_T -> IType_T) -> IType -> IType
modifyIType = coerce

scalarType :: String -> IType
scalarType name = IType defaultIType { name = Just name }

unnamed :: IT.ITypeKind -> IType
unnamed kind = IType defaultIType { kind = kind }

getTypeWithNull :: forall a. Gql a => Proxy a -> IType
getTypeWithNull proxy =
  if isNullable proxy then
    iType unit
  else
    unnamed IT.NON_NULL # modifyIType _ { ofType = Just $ iType unit }
  where
  GqlProps { iType } = (gql unit) :: GqlProps a

getTypeWithoutNull :: forall a. Gql a => Proxy a -> IType
getTypeWithoutNull _proxy = iType unit
  where
  GqlProps { iType } = (gql unit) :: GqlProps a