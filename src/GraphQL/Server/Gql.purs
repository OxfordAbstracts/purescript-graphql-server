module GraphQL.Server.Gql where

import GraphQL.Server.Branch
import Prelude

import Control.Apply (lift2)
import Control.Monad.Fork.Class as Fork
import Control.Monad.Reader (ask)
import Data.Argonaut (class EncodeJson, Json, encodeJson)
import Data.Argonaut.Encode.Generic (class EncodeLiteral, encodeLiteralSum)
import Data.Date (Date)
import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.Filterable (filter)
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), Sum(..), from)
import Data.Lazy (Lazy, force)
import Data.List (List(..), reverse, (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Time (Time)
import Data.Traversable (sequence)
import Effect.Aff (Aff, Fiber)
import Effect.Aff.Class (liftAff)
import GraphQL.Server.Auth (Forbidden(..), forbiddenName, forbiddenType, isForbidden)
import GraphQL.Server.DateTime (encodeDate, encodeDateTime, encodeTime)
import GraphQL.Server.Decode (class DecodeArg, decodeArg)
import GraphQL.Server.GqlError (FailedToResolve(..))
import GraphQL.Server.GqlM (GqlM, gqlToAff)
import GraphQL.Server.Introspection.GqlNullable (class GqlNullable, isNullable)
import GraphQL.Server.Introspection.Types (IDirective, IEnumValue, IField(..), IInputValue(..), ISchema, IType(..), ITypeKind, defaultIType)
import GraphQL.Server.Introspection.Types as IT
import GraphQL.Server.Introspection.Types.DirectiveLocation (IDirectiveLocation)
import GraphQL.Server.Record.Unsequence (class UnsequenceProxies, unsequenceProxies)
import GraphQL.Server.Resolver.JsonResolver (Field, Resolver(..))
import HTTPure (Request)
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Safe.Coerce (coerce)
import Type.Proxy (Proxy(..))

class GqlNullable a <= Gql env a | a -> env where
  gql :: Unit -> GqlProps env a

gql' :: forall env a. Gql env a => GqlPropsT env a
gql' = unwrap $ gql unit

newtype GqlProps env a = GqlProps (GqlPropsT env a)

derive instance Newtype (GqlProps env a) _

type GqlPropsT env a =
  { resolver :: a -> Request -> Resolver env
  , iType :: Unit -> GqlM env IType
  }

getIType :: forall env a. GqlProps env a -> Unit -> GqlM env IType
getIType (GqlProps { iType }) _ = iType unit

getResolver :: forall a env. GqlProps env a -> a -> Request -> Resolver env
getResolver (GqlProps { resolver }) = resolver

toResolver :: forall a env. Gql env a => a -> Request -> Resolver env
toResolver a req = resolver a req
  where
  (GqlProps { resolver }) = gql unit

instance Gql env Void where
  gql _ = jsonScalar "Void"

instance Gql env Unit where
  gql _ = jsonScalar "Unit"

instance Gql env Boolean where
  gql _ = jsonScalar "Boolean"

instance Gql env Int where
  gql _ = jsonScalar "Int"

instance Gql env Number where
  gql _ = jsonScalar "Float"

instance Gql env String where
  gql _ = jsonScalar "String"

instance IsSymbol sym => Gql env (Proxy sym) where
  gql _ = scalar (reflectSymbol >>> encodeJson) "String"

instance Gql env Json where
  gql _ = jsonScalar "Json"

instance Gql env Date where
  gql _ = scalar encodeDate "Date"

instance Gql env Time where
  gql _ = scalar encodeTime "Time"

instance Gql env DateTime where
  gql _ = scalar encodeDateTime "DateTime"

instance (GqlNullable a, Gql env a) => Gql env (Maybe a) where
  gql _ = GqlProps
    { resolver: \a req ->
        maybe Null (flip resolver req) a
    , iType
    }
    where
    GqlProps { resolver, iType } = (gql unit) :: GqlProps env a

instance Gql env a => Gql env (Array a) where
  gql _ = GqlProps
    { resolver: \a req -> ListResolver $ flip resolver req <$> List.fromFoldable a
    , iType: \_ -> do
        t <- getTypeWithNull (Proxy :: Proxy a)
        pure $ IT.unnamed IT.LIST # IT.modifyIType _
          { ofType = Just t
          }
    }
    where
    GqlProps { resolver } = (gql unit) :: GqlProps env a

instance Gql env a => Gql env (List a) where
  gql _ = GqlProps
    { resolver: \a req -> ListResolver $ flip resolver req <$> a
    , iType: \_ -> do
        t <- getTypeWithNull (Proxy :: Proxy a)
        pure $ IT.unnamed IT.LIST # IT.modifyIType _
          { ofType = Just t
          }
    }
    where
    GqlProps { resolver } = (gql unit) :: GqlProps env a

instance (Gql env a) => Gql env (GqlM env a) where
  gql _ = GqlProps
    { resolver: \a req ->
        AsyncResolver do
          a' <- a
          let
            GqlProps { resolver } = gql unit :: GqlProps env a
          pure $ resolver a' req
    , iType: \_ ->
        let
          GqlProps { iType } = gql unit :: GqlProps env a
        in
          iType unit
    }

instance (Gql env a) => Gql env (Aff a) where
  gql _ = GqlProps
    { resolver: \a req ->
        AsyncResolver do
          a' <- liftAff a
          let
            GqlProps { resolver } = gql unit :: GqlProps env a
          pure $ resolver a' req
    , iType: \_ ->
        let
          GqlProps { iType } = gql unit :: GqlProps env a
        in
          iType unit
    }

instance (Gql env a) => Gql env (Fiber a) where
  gql _ = GqlProps
    { resolver: \a req ->
        AsyncResolver do
          a' <- Fork.join a
          let
            GqlProps { resolver } = gql unit :: GqlProps env a
          pure $ resolver a' req
    , iType: \_ ->
        let
          GqlProps { iType } = gql unit :: GqlProps env a
        in
          iType unit
    }

instance (Gql env a) => Gql env (Lazy a) where
  gql _ = GqlProps
    { resolver: \a req ->
        let
          GqlProps { resolver } = gql unit :: GqlProps env a
        in
          resolver (force a) req

    , iType: \_ ->
        let
          GqlProps { iType } = gql unit :: GqlProps env a
        in
          iType unit
    }

instance (Gql env a, Gql env b, GqlIf pred env) => Gql env (Branch pred a b) where
  gql _ = GqlProps
    { resolver: \(Branch _ a b) req ->
        AsyncResolver do
          pass <- gqlIf (Proxy :: Proxy pred)
          if pass then do
            let
              GqlProps { resolver } = gql unit :: GqlProps env b
            pure $ resolver b req
          else do
            let
              GqlProps { resolver } = gql unit :: GqlProps env a
            pure $ resolver a req

    , iType: \_ ->
        do
          pass <- gqlIf (Proxy :: Proxy pred)
          if pass then do
            let
              GqlProps { iType } = gql unit :: GqlProps env b

            iType unit
          else do
            let
              GqlProps { iType } = gql unit :: GqlProps env a

            iType unit
    }

instance Gql env Forbidden where
  gql _ = GqlProps
    { resolver: \_ _ -> Null
    , iType: \_ -> pure $ forbiddenType
    }

instance Gql env ISchema where
  gql = objectWithName "__Schema"

instance Gql env IDirective where
  gql = objectWithName "__Directive"

instance Gql env IDirectiveLocation where
  gql = enum "__DirectiveLocation"

instance Gql env IInputValue where
  gql _ = objectWithName "__InputValue" unit

instance Gql env IType where
  gql _ = objectWithName "__Type" unit

instance Gql env IField where
  gql = objectWithName "__Field"

instance Gql env IEnumValue where
  gql = objectWithName "__EnumValue"

instance Gql env ITypeKind where
  gql = enum "__TypeKind"

-- | create an enum graphql type
enum
  :: forall rep a env
   . Generic a rep
  => EncodeLiteral rep
  => String
  -> Unit
  -> GqlProps env a
enum name _ = GqlProps
  { resolver: \a _ -> Node $ pure $ encodeLiteralSum a
  , iType: \_ -> pure $ IType defaultIType
      { name = Just name
      , kind = IT.ENUM
      }
  }

-- | create a union graphql type
union
  :: forall rep a env
   . Generic a rep
  => UnionGql env rep
  => String
  -> Unit
  -> GqlProps env a
union name _ = GqlProps
  { resolver: \a req -> unionResolver name (from a) req
  , iType: \_ -> do
      possible <- possibleTypes name (Proxy :: Proxy rep)
      pure $ IType defaultIType
        { name = Just name
        , kind = IT.UNION
        , possibleTypes = Just $ possible
        }
  }

-- class UnionGql :: forall k. Type -> k -> Constraint
class UnionGql env a where
  unionResolver :: String -> a -> Request -> Resolver env
  possibleTypes :: String -> Proxy a -> GqlM env (List IType)

instance
  ( UnionGql env l
  , UnionGql env r
  ) =>
  UnionGql env (Sum l r) where
  unionResolver name a req = case a of
    Inl l -> unionResolver name l req
    Inr r -> unionResolver name r req
  possibleTypes name _ = lift2 (<>) (possibleTypes name (Proxy :: Proxy l)) (possibleTypes name (Proxy :: Proxy r))

instance
  ( HFoldlWithIndex (ToResolverProps env) (FieldMap env) { | arg } (FieldMap env)
  , GetIFields env { | arg }
  , IsSymbol name
  ) =>
  UnionGql env (Constructor name (Argument { | arg })) where
  unionResolver ctrName (Constructor (Argument a)) req =
    Fields
      { fields: makeFields typename req a
      , typename
      }
    where
    typename = ctrName <> "_" <> reflectSymbol (Proxy :: Proxy name)
  possibleTypes ctrName _ = do
    env <- ask
    pure $ pure $ IType defaultIType
      { name = Just typename
      , kind = IT.OBJECT
      , fields = \_ -> do
          fields <- gqlToAff env $ getIFields (Proxy :: Proxy { | arg })
          pure $ Just fields
      }
    where
    typename = ctrName <> "_" <> reflectSymbol (Proxy :: Proxy name)

else instance
  ( Gql env arg
  ) =>
  UnionGql env (Constructor name (Argument arg)) where
  unionResolver _ctrName a req =
    let
      (GqlProps { resolver }) = gql unit :: GqlProps env arg
    in
      resolver (coerce a) req
  possibleTypes _ _ = do
    let
      (GqlProps { iType }) = gql unit :: GqlProps env arg

    pure <$> iType unit

-- | create an object graphql type
object
  :: forall r name a env
   . Generic a (Constructor name (Argument { | r }))
  => GqlObject env a
  => IsSymbol name
  => Unit
  -> GqlProps env a
object = objectWithName $ reflectSymbol (Proxy :: Proxy name)

-- | create an object graphql type with a custom name
objectWithName
  :: forall r name a env
   . Generic a (Constructor name (Argument { | r }))
  => GqlObject env a
  => String
  -> Unit
  -> GqlProps env a
objectWithName typename _ = gqlObject typename

class GqlObject env a where
  gqlObject :: String -> GqlProps env a

instance
  ( Generic a (Constructor name (Argument { | r }))
  , HFoldlWithIndex (ToResolverProps env) (FieldMap env) { | r } (FieldMap env)
  , GetIFields env { | r }
  ) =>
  GqlObject env a where
  gqlObject typename = makeGqlObject (Proxy :: Proxy env) (Proxy :: Proxy { | r }) typename

makeGqlObject
  :: forall arg a name r env
   . Generic a (Constructor name (Argument arg))
  => HFoldlWithIndex (ToResolverProps env) (FieldMap env) arg (FieldMap env)
  => GetIFields env r
  => Proxy env
  -> Proxy r
  -> String
  -> GqlProps env a
makeGqlObject _ recordProxy typename = GqlProps
  { resolver: \a req ->
      let
        Constructor (Argument r) = from a
      in
        Fields
          { fields: makeFields typename req r
          , typename
          }
  , iType: \_ -> do
      env <- ask
      pure $ IType defaultIType
        { name = Just typename
        , kind = IT.OBJECT
        , fields = \_ -> do
            fields <- gqlToAff env $ getIFields recordProxy
            pure $ Just fields
        }
  }

makeFields
  :: forall a env
   . HFoldlWithIndex (ToResolverProps env) (FieldMap env) a (FieldMap env)
  => String
  -> Request
  -> a
  -> Map String (Field env)
makeFields typename req r =
  unwrap $ hfoldlWithIndex (ToResolverProps req :: ToResolverProps env) resolveTypename r
  where
  resolveTypename :: FieldMap env
  resolveTypename = FieldMap $ Map.singleton "__typename"
    { name: "__typename"
    , resolver: \_ -> Node $ pure $ encodeJson typename
    }

data ToResolverProps :: forall k. k -> Type
data ToResolverProps env = ToResolverProps Request

newtype FieldMap env = FieldMap (Map String (Field env))

derive instance Newtype (FieldMap env) _

instance
  ( IsSymbol sym
  , GetArgResolver env a
  ) =>
  FoldingWithIndex (ToResolverProps env) (Proxy sym) (FieldMap env) a (FieldMap env) where
  foldingWithIndex (ToResolverProps req) prop (FieldMap fieldMap) a =
    FieldMap $ Map.insert name field fieldMap
    where
    name = reflectSymbol prop

    field :: Field env
    field =
      { name
      , resolver: getArgResolver a req
      }

class GetArgResolver env a where
  getArgResolver
    :: a
    -> Request
    -> { args :: Json }
    -> Resolver env

instance argResolverUnitFn :: Gql env a => GetArgResolver env (Unit -> a) where
  getArgResolver a req _ = toResolver (a unit) req

else instance argResolverAllFn :: (DecodeArg a, Gql env b) => GetArgResolver env (a -> b) where
  getArgResolver fn req { args } = case decodeArg args of
    Left err -> FailedResolver $ ResolverDecodeError err
    Right a -> toResolver (fn a) req

else instance argResolverAny :: Gql env a => GetArgResolver env a where
  getArgResolver a req _ = toResolver a req

class GetIFields :: forall k. Type -> k -> Constraint
class GetIFields env a where
  getIFields :: Proxy a -> GqlM env (List IField)

instance
  ( HFoldlWithIndex GetIFieldsProps (List (GqlM env IField)) { | p } (List (GqlM env IField))
  , UnsequenceProxies { | r } { | p }
  ) =>
  GetIFields env { | r } where
  getIFields r = (getRecordIFields ((unsequenceProxies r) :: { | p }))

data GetIFieldsProps = GetIFieldsProps

instance
  ( IsSymbol label
  , Gql env b
  , GetIInputValues env (a -> b)
  ) =>
  FoldingWithIndex GetIFieldsProps (Proxy label) (List (GqlM env IField)) (Proxy (a -> b)) (List (GqlM env IField)) where
  foldingWithIndex GetIFieldsProps sym defs a = def : defs
    where
    def = do
      args <- getIInputValues a
      t <- getTypeWithNull (Proxy :: Proxy b)
      pure $ IField
        { args
        , deprecationReason: Nothing
        , description: Nothing
        , isDeprecated: false
        , name: reflectSymbol sym
        , type: t
        }
else instance
  ( IsSymbol label
  , Gql env a
  , GetIInputValues env a
  ) =>
  FoldingWithIndex GetIFieldsProps (Proxy label) (List (GqlM env IField)) (Proxy a) (List (GqlM env IField)) where
  foldingWithIndex GetIFieldsProps sym defs a = def : defs
    where
    def = do
      args <- getIInputValues a
      t <- getTypeWithNull a
      pure $ IField
        { args
        , deprecationReason: Nothing
        , description: Nothing
        , isDeprecated: false
        , name: reflectSymbol sym
        , type: t
        }

getRecordIFields
  :: forall r env
   . HFoldlWithIndex GetIFieldsProps (List (GqlM env IField)) { | r } (List (GqlM env IField))
  => { | r }
  -> GqlM env (List IField)
getRecordIFields r =
  r # hfoldlWithIndex (GetIFieldsProps :: GetIFieldsProps) (mempty :: List (GqlM env IField))
    # reverse
    # sequence
    <#> filter (unwrap >>> _.type >>> not isForbidden)

class GetIInputValues :: forall k. Type -> k -> Constraint
class GetIInputValues env a where
  getIInputValues :: Proxy a -> GqlM env (List IInputValue)

instance
  ( HFoldlWithIndex (GetIInputValuesProps) (List (GqlM env IInputValue)) { | p } (List (GqlM env IInputValue))
  , UnsequenceProxies { | r } { | p }
  ) =>
  GetIInputValues env ({ | r } -> a) where
  getIInputValues _r = getRecordIInputValues ((unsequenceProxies (Proxy :: Proxy { | r })) :: { | p })
else instance
  GetIInputValues env a where
  getIInputValues _r = pure $ Nil

data GetIInputValuesProps = GetIInputValuesProps

instance
  ( IsSymbol label
  , Gql env a
  ) =>
  FoldingWithIndex (GetIInputValuesProps) (Proxy label) (List (GqlM env IInputValue)) (Proxy a) (List (GqlM env IInputValue)) where
  foldingWithIndex (GetIInputValuesProps) sym defs a = def : defs
    where
    def = do
      t <- getTypeWithNull a
      pure $ IInputValue
        { name: reflectSymbol sym
        , description: Nothing
        , type: t
        , defaultValue: Nothing
        }

getRecordIInputValues
  :: forall r env
   . HFoldlWithIndex GetIInputValuesProps (List (GqlM env IInputValue)) { | r } (List (GqlM env IInputValue))
  => { | r }
  -> GqlM env (List IInputValue)
getRecordIInputValues =
  hfoldlWithIndex (GetIInputValuesProps :: GetIInputValuesProps) (mempty :: List (GqlM env IInputValue))
    >>> reverse
    >>> sequence

jsonScalar :: forall env a. EncodeJson a => String -> GqlProps env a
jsonScalar = scalar encodeJson

scalar :: forall env a. (a -> Json) -> String -> GqlProps env a
scalar encode name = GqlProps
  { resolver: \a _ -> Node $ pure $ encode a
  , iType: \_ -> pure $ IT.scalarType name
  }

getTypeWithNull :: forall env a. Gql env a => Proxy a -> GqlM env IType
getTypeWithNull proxy =
  if isNullable proxy then
    iType unit
  else do
    t <- iType unit
    pure $ IT.unnamed IT.NON_NULL # IT.modifyIType _ { ofType = Just t }
  where
  GqlProps { iType } = (gql unit) :: GqlProps env a

getTypeWithoutNull :: forall env a. Gql env a => Proxy a -> GqlM env IType
getTypeWithoutNull _proxy = iType unit
  where
  -- _ = spy "getTypeWithoutNull" $ reflectType _proxy
  GqlProps { iType } = (gql unit) :: GqlProps env a