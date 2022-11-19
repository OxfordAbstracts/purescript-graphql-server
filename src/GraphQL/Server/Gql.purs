module GraphQL.Server.Gql where

import Prelude

import Data.Argonaut (class EncodeJson, Json, encodeJson)
import Data.Argonaut.Encode.Generic (class EncodeLiteral, encodeLiteralSum)
import Data.Date (Date)
import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), Sum(..), from)
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Time (Time)
import Effect.Aff (Aff)
import Effect.Exception (Error)
import GraphQL.Resolver.JsonResolver (AffResolver, Field, Resolver(..))
import GraphQL.Server.DateTime (encodeDate, encodeDateTime, encodeTime)
import GraphQL.Server.Decode (class DecodeArg, decodeArg)
import GraphQL.Server.GqlError (FailedToResolve(..))
import GraphQL.Server.Schema.Introspection.GetType (class GetIFields, getIFields)
import GraphQL.Server.Schema.Introspection.GqlNullable (class GqlNullable, isNullable)
import GraphQL.Server.Schema.Introspection.Types (IType(..), IType_T, defaultIType)
import GraphQL.Server.Schema.Introspection.Types as IT
import HTTPure (Request)
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Safe.Coerce (coerce)
import Type.Proxy (Proxy(..))

class GqlNullable a <= Gql a where
  gql :: GqlProps a

newtype GqlProps a = GqlProps
  { resolver :: a -> Request -> AffResolver
  , iType :: IType
  }

getIType :: forall a. GqlProps a -> IType
getIType (GqlProps { iType }) = iType

getResolver :: forall a. GqlProps a -> a -> Request -> AffResolver
getResolver (GqlProps { resolver }) = resolver

toResolver :: forall a. Gql a => a -> Request -> AffResolver
toResolver = resolver
  where
  (GqlProps { resolver }) = gql

instance Gql Void where
  gql = jsonScalar "Void"

instance Gql Unit where
  gql = jsonScalar "Unit"

instance Gql Boolean where
  gql = jsonScalar "Boolean"

instance Gql Int where
  gql = jsonScalar "Int"

instance Gql Number where
  gql = jsonScalar "Float"

instance Gql String where
  gql = jsonScalar "String"

instance Gql Json where
  gql = jsonScalar "Json"

instance Gql Date where
  gql = scalarWith encodeDate "Date"

instance Gql Time where
  gql = scalarWith encodeTime "Time"

instance Gql DateTime where
  gql = scalarWith encodeDateTime "DateTime"

instance Gql a => Gql (Maybe a) where
  gql = GqlProps
    { resolver: \a req -> NullableResolver $ flip resolver req <$> a
    , iType
    }
    where
    GqlProps { resolver, iType } = gql :: GqlProps a

instance Gql a => Gql (Array a) where
  gql = GqlProps
    { resolver: \a req -> ListResolver $ flip resolver req <$> List.fromFoldable a
    , iType: unnamed IT.LIST # modifyIType _
        { ofType = Just $ getTypeWithNull (Proxy :: Proxy a)
        }
    }
    where
    GqlProps { resolver } = gql :: GqlProps a

instance Gql a => Gql (List a) where
  gql = GqlProps
    { resolver: \a req -> ListResolver $ flip resolver req <$> a
    , iType: unnamed IT.LIST # modifyIType _
        { ofType = Just $ getTypeWithNull (Proxy :: Proxy a)
        }
    }
    where
    GqlProps { resolver } = gql :: GqlProps a

-- | create an enum graphql type
enum
  :: forall rep a
   . Generic a rep
  => EncodeLiteral rep
  => String
  -> GqlProps a
enum name = GqlProps
  { resolver: \a _ -> Node $ pure $ encodeLiteralSum a
  , iType: IType defaultIType
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
  -> GqlProps a
union name = GqlProps
  { resolver: unionResolver
  , iType: IType defaultIType
      { name = Just name
      , kind = IT.UNION
      , possibleTypes = Just $ possibleTypes (Proxy :: Proxy rep)
      }
  }

class Generic a rep <= UnionGql a rep | rep -> a, a -> rep where
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
  unionResolver req = getResolver (gql :: GqlProps a) req
  possibleTypes _ = pure $ getIType (gql :: GqlProps a)

else instance
  ( Generic a (Constructor name arg)
  , Gql a
  ) =>
  UnionGql a (Constructor name arg) where
  unionResolver req = getResolver (gql :: GqlProps a) req
  possibleTypes _ = pure $ getIType (gql :: GqlProps a)

-- | create an object graphql type
object
  :: forall r name a
   . Generic a (Constructor name (Argument { | r }))
  => HFoldlWithIndex ToResolverProps FieldMap { | r } FieldMap
  => GetIFields { | r }
  => IsSymbol name
  => GqlProps a
object = objectWithName $ reflectSymbol (Proxy :: Proxy name)

-- | create an object graphql type with a custom name
objectWithName
  :: forall r name a
   . Generic a (Constructor name (Argument { | r }))
  => HFoldlWithIndex ToResolverProps FieldMap { | r } FieldMap
  => GetIFields { | r }
  => String
  -> GqlProps a
objectWithName typename = GqlProps
  { resolver: \a req ->
      let
        (Constructor (Argument r)) = from a
      in
        Fields
          { fields: makeFields req r
          , typename
          }
  , iType: IType defaultIType
      { name = Just typename
      , kind = IT.OBJECT
      , fields = \_ -> Just $ getIFields (Proxy :: Proxy { | r })
      }
  }
  where
  makeFields req r =
    unwrap $ hfoldlWithIndex (ToResolverProps req) resolveTypename r
    where
    resolveTypename = FieldMap $ Map.singleton "__typename"
      { name: "__typename"
      , resolver: \_ -> Node $ pure $ encodeJson typename
      }

data ToResolverProps = ToResolverProps Request

newtype FieldMap = FieldMap (Map String (Field Error Aff))

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

    field :: Field Error Aff
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
  getArgResolver a req = \_ -> toResolver  (a unit) req

else instance argResolverAllFn :: (DecodeArg a, Gql b) => GetArgResolver (a -> b) where
  getArgResolver  fn req { args } = case decodeArg args of
    Left err -> FailedResolver $ ResolverDecodeError err
    Right a -> toResolver (fn a) req

else instance argResolverAny :: Gql a => GetArgResolver a where
  getArgResolver a req = \_ -> toResolver a req

jsonScalar :: forall a. EncodeJson a => String -> GqlProps a
jsonScalar = scalarWith encodeJson

scalarWith :: forall a. (a -> Json) -> String -> GqlProps a
scalarWith encode name = GqlProps
  { resolver: \a _ -> Node $ pure $ encode a
  , iType: scalarType name
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
    iType
  else
    unnamed IT.NON_NULL # modifyIType _ { ofType = Just iType }
  where
  GqlProps { iType } = gql :: GqlProps a
