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
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Time (Time)
import Effect.Aff (Aff)
import Effect.Exception (Error)
import GraphQL.Record.Unsequence (class UnsequenceProxies, unsequenceProxies)
import GraphQL.Resolver.GqlIo (GqlIo)
import GraphQL.Resolver.JsonResolver (AffResolver, Field, Resolver(..))
import GraphQL.Server.DateTime (encodeDate, encodeDateTime, encodeTime)
import GraphQL.Server.Decode (class DecodeArg, decodeArg)
import GraphQL.Server.GqlError (FailedToResolve(..))
import GraphQL.Server.Schema.Introspection.GqlNullable (class GqlNullable, isNullable)
import GraphQL.Server.Schema.Introspection.Types (IDirective, IEnumValue, IField(..), IInputValue(..), ISchema, IType(..), ITypeKind, IType_T, defaultIType)
import GraphQL.Server.Schema.Introspection.Types as IT
import GraphQL.Server.Schema.Introspection.Types.DirectiveLocation (IDirectiveLocation)
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
  { resolver :: a -> Request -> AffResolver
  , iType :: Unit -> IType
  }

getIType :: forall a. GqlProps a -> Unit -> IType
getIType (GqlProps { iType }) _ = iType unit

getResolver :: forall a. GqlProps a -> a -> Request -> AffResolver
getResolver (GqlProps { resolver }) = resolver

toResolver :: forall a. Gql a => a -> Request -> AffResolver
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

instance Gql Json where
  gql _ = jsonScalar "Json"

instance Gql Date where
  gql _ = scalarWith encodeDate "Date"

instance Gql Time where
  gql _ = scalarWith encodeTime "Time"

instance Gql DateTime where
  gql _ = scalarWith encodeDateTime "DateTime"

instance Gql a => Gql (Maybe a) where
  gql _ = GqlProps
    { resolver: \a req -> NullableResolver $ flip resolver req <$> a
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

instance (Gql a) => Gql (GqlIo Aff a) where
  gql _ = GqlProps
    { resolver: \a req ->
        let
          GqlProps { resolver } = gql unit :: GqlProps a
        in
          AsyncResolver do
            a' <- a
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
  => UnionGql a rep
  => String
  -> Unit
  -> GqlProps a
union name _ = GqlProps
  { resolver: unionResolver
  , iType: \_ -> IType defaultIType
      { name = Just name
      , kind = IT.UNION
      , possibleTypes = Just $ possibleTypes (Proxy :: Proxy rep)
      }
  }

class UnionGql a rep | rep -> a, a -> rep where
  unionResolver :: a -> Request -> AffResolver
  possibleTypes :: Proxy rep -> List IType

instance
  ( Generic a (Sum l r)
  , UnionGql l lrep
  , UnionGql r rrep
  ) =>
  UnionGql a (Sum l r) where
  unionResolver a req = case from a of
    Inl l -> unionResolver l req
    Inr r -> unionResolver r req
  possibleTypes _ = possibleTypes (Proxy :: Proxy lrep) <> possibleTypes (Proxy :: Proxy rrep)

else instance
  ( Generic a (Constructor name { | arg })
  , Gql a
  ) =>
  UnionGql a (Constructor name { | arg }) where
  unionResolver req = getResolver ((gql unit) :: GqlProps a) req
  possibleTypes _ = pure $ getIType ((gql unit) :: GqlProps a) unit

else instance
  ( Generic a (Constructor name arg)
  , Gql a
  ) =>
  UnionGql a (Constructor name arg) where
  unionResolver req = getResolver ((gql unit) :: GqlProps a) req
  possibleTypes _ = pure $ getIType ((gql unit) :: GqlProps a) unit

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
   . HFoldlWithIndex ToResolverProps FieldMap arg FieldMap
  => Generic a (Constructor name (Argument arg))
  => GetIFields r
  => Proxy r
  -> String
  -> GqlProps a
makeGqlObject recordProxy typename = GqlProps
  { resolver: \a req ->
      let
        (Constructor (Argument r)) = from a
      in
        Fields
          { fields: makeFields req r
          , typename
          }
  , iType: \_ -> IType defaultIType
      { name = Just typename
      , kind = IT.OBJECT
      , fields = \_ -> Just $ getIFields recordProxy
      }
  }
  where
  makeFields req r =
    (unwrap :: FieldMap -> _) $ hfoldlWithIndex (ToResolverProps req) resolveTypename r
    where
    resolveTypename = FieldMap $ Map.singleton "__typename"
      { name: "__typename"
      , resolver: \_ -> Node $ pure $ encodeJson typename
      }

data ToResolverProps = ToResolverProps Request

newtype FieldMap = FieldMap (Map String (Field Error (GqlIo Aff)))

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

    field :: Field Error (GqlIo Aff)
    field =
      { name
      , resolver: getArgResolver a req
      }

class GetArgResolver a where
  getArgResolver
    :: a
    -> Request
    -> { args :: Json }
    -> AffResolver

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
jsonScalar = scalarWith encodeJson

scalarWith :: forall a. (a -> Json) -> String -> GqlProps a
scalarWith encode name = GqlProps
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
