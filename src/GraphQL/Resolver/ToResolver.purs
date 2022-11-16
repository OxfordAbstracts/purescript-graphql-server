module GraphQL.Resolver.ToResolver
  ( FieldMap(..)
  , ToResolverProps(..)
  , class GenericUnionResolver
  , class GetArgResolver
  , class ToResolver
  , class ToResolverGeneric
  , genericUnionResolver
  , getArgResolver
  , makeFields
  , toEnumResolver
  , toObjectResolver
  , toResolver
  , toResolverGeneric
  , toScalarResolver
  , toUnionResolver
  ) where

import Prelude

import Data.Argonaut (class EncodeJson, Json, encodeJson)
import Data.Argonaut.Encode.Generic (class EncodeLiteral, encodeLiteralSum)
import Data.Date (Date)
import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments, Sum, from)
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Time (Time)
import Effect.Aff (Aff)
import Effect.Exception (Error)
import GraphQL.Resolver.EvalGql (class EvalGql, evalGql)
import GraphQL.Resolver.GqlIo (GqlIo)
import GraphQL.Resolver.InstanceCache (Cons(..), Nil(..))
import GraphQL.Resolver.JsonResolver (Field, Resolver(..))
import GraphQL.Resolver.Root (GqlRoot(..), MutationRoot, QueryRoot)
import GraphQL.Server.DateTime (encodeDate, encodeDateTime, encodeTime)
import GraphQL.Server.Decode (class DecodeArg, decodeArg)
import GraphQL.Server.GqlError (FailedToResolve(..))
import GraphQL.Server.GqlRep (class GqlRep, GEnum, GObject, GUnion)
import GraphQL.Server.Schema.RecordTypename (class RecordTypename)
import GraphQL.Server.Schema.Scalar (class Scalar, encodeScalar)
import HTTPure (Request)
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Prim.RowList (class RowToList)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

class ToResolver iCache a where
  toResolver :: iCache -> Request -> a -> AffResolver

type AffResolver = Resolver Error Aff

unsafeResolveNode :: forall a. EncodeJson a => Request -> a -> AffResolver
unsafeResolveNode _ a = Node $ pure $ encodeJson a

unsafeResolveNodeNoReq :: forall a. EncodeJson a => a -> AffResolver
unsafeResolveNodeNoReq a = Node $ pure $ encodeJson a

toAsyncResolver :: forall c m a. EvalGql m => ToResolver c a => c -> Request -> m a -> AffResolver
toAsyncResolver c req a = ResolveAsync $ toResolver c req <$> evalGql req a

instance (EvalGql m, ToResolver c a) => ToResolver c (GqlIo m a) where
  toResolver iCache a = toAsyncResolver iCache a

else instance ToResolver c Boolean where
  toResolver _iCache a = unsafeResolveNode a

else instance ToResolver c Int where
  toResolver _iCache a = unsafeResolveNode a

else instance ToResolver c Number where
  toResolver _iCache a = unsafeResolveNode a

else instance ToResolver c String where
  toResolver _iCache a = unsafeResolveNode a

else instance ToResolver c Date where
  toResolver _iCache a = unsafeResolveNodeWith encodeDate a

else instance ToResolver c Time where
  toResolver _iCache a = unsafeResolveNodeWith encodeTime a

else instance ToResolver c DateTime where
  toResolver _iCache a = unsafeResolveNodeWith encodeDateTime a

else instance ToResolver c Json where
  toResolver _iCache a = unsafeResolveNode a

else instance ToResolver c Unit where
  toResolver _iCache a = unsafeResolveNode a

else instance ToResolver c Void where
  toResolver _iCache a = unsafeResolveNode a

else instance (ToResolver c a) => ToResolver c (List a) where
  toResolver iCache req a = ListResolver $ toResolver iCache req <$> a

else instance (ToResolver c a) => ToResolver c (Maybe a) where
  toResolver iCache req a = NullableResolver $ toResolver iCache req <$> a

else instance (ToResolver c a) => ToResolver c (Array a) where
  toResolver iCache req a = ListResolver $ map (toResolver iCache req) $ List.fromFoldable a

else instance ToResolver c a => ToResolver c (Unit -> a) where
  toResolver iCache req a = toResolver iCache req $ a unit

else instance (IsSymbol sym) => ToResolver c (Proxy sym) where
  toResolver _iCache req _ = unsafeResolveNode req (reflectSymbol (Proxy :: Proxy sym))
else instance
  ( HFoldlWithIndex (ToResolverProps c) FieldMap ({ query :: q, mutation :: mut }) FieldMap
  , IsSymbol "root"
  ) =>
  ToResolver c (GqlRoot q mut) where
  toResolver iCache req (GqlRoot root) = Fields
    { fields: makeFields iCache req (reflectSymbol (Proxy :: Proxy "root")) root
    , typename: "root"
    }
else instance
  ( HFoldlWithIndex (ToResolverProps c) FieldMap { | a } FieldMap
  ) =>
  ToResolver c (QueryRoot { | a }) where
  toResolver iCache a = toObjectResolver iCache a
else instance
  ( HFoldlWithIndex (ToResolverProps c) FieldMap { | a } FieldMap
  ) =>
  ToResolver c (MutationRoot { | a }) where
  toResolver iCache req a = toObjectResolver iCache req a

else instance
  ToResolver c (MutationRoot Unit) where
  toResolver _iCache _ _ = FailedResolver NoMutationRoot
else instance
  ( RowToList r l
  , RecordTypename { | r } name
  , HFoldlWithIndex (ToResolverProps c) FieldMap { | r } FieldMap
  , IsSymbol name
  ) =>
  ToResolver c { | r } where
  toResolver iCache req arg =
    Fields
      { fields: makeFields iCache req (reflectSymbol (Proxy :: Proxy name)) arg
      , typename: reflectSymbol (Proxy :: Proxy name)
      }
else instance
  ( Generic a rep
  , ToResolverGeneric c c a rep name
  ) =>
  ToResolver c a where
  toResolver iCache req arg = toResolverGeneric iCache iCache req arg (from arg)

-- keep a list of the types already visited
-- and make a lazy resolver that can be cached for 
-- future use



class ToResolverGeneric :: Type -> Type -> Type -> Type -> Symbol -> Constraint
class
  ToResolverGeneric fullCache iCache a rep name
  |
    -- name -> a rep
    -- , 
    a -> rep
  -- -- , rep -> a
  -- -- , a -> a
  -- , 
  , a -> name
  -- , iCache -> a 
  -- , fullCache -> a 
  -- , a -> iCache
  -- , a -> fullCache 
  where
  toResolverGeneric :: fullCache -> iCache -> Request -> a -> rep -> AffResolver

instance resolverCached ::
  ToResolverGeneric c (Cons a rep rest) a rep name where
  toResolverGeneric _c (Cons fn _) req a rep = fn req a rep
else instance resolverRest ::
  ( ToResolverGeneric c rest a rep name
  ) =>
  ToResolverGeneric c (Cons notA notRep rest) a rep name where
  toResolverGeneric c (Cons _ rest) req a rep = toResolverGeneric c rest req a rep

instance resolverObject ::
  ( IsSymbol name
  , HFoldlWithIndex
      ( ToResolverProps
          ( Cons { | arg }
              (Constructor name (Argument { | arg }))
              c
          )
      )
      FieldMap
      { | arg }
      FieldMap
  ) =>
  ToResolverGeneric c Nil a (Constructor name (Argument { | arg })) name where
  toResolverGeneric c _ req _ rep = getRes req rep
    where
    getRes :: Request -> Constructor name (Argument { | arg }) -> AffResolver
    getRes req' rep' =
      unsafeToObjectResolverRep
        ((Cons (\req'' _a' rep'' -> getRes req'' rep'') c) :: (Cons { | arg } (Constructor name (Argument { | arg })) c))
        req'
        (Proxy :: Proxy name)
        rep'

else instance resolverEnum ::
  ( IsSymbol name
  , Generic a (Sum (Constructor cName NoArguments) r)
  , GqlRep a GEnum name
  , EncodeLiteral (Sum (Constructor cName NoArguments) r)
  ) =>
  ToResolverGeneric c Nil a (Sum (Constructor cName NoArguments) r) name where
  toResolverGeneric _ _ _ a _ = toEnumResolver a

toScalarResolver
  :: forall a name
   . Scalar a name
  => Request
  -> a
  -> AffResolver
toScalarResolver = unsafeResolveNodeWith encodeScalar

toEnumResolver
  :: forall a name cName r
   . Generic a (Sum (Constructor cName NoArguments) r)
  => EncodeLiteral (Sum (Constructor cName NoArguments) r)
  => GqlRep a GEnum name
  => a
  -> AffResolver
toEnumResolver = unsafeResolveNodeWithNoReq encodeLiteralSum

toObjectResolver
  :: forall a arg name ctrName c
   . Generic a (Constructor ctrName (Argument { | arg }))
  => GqlRep a GObject name
  => IsSymbol name
  => HFoldlWithIndex (ToResolverProps c) FieldMap { | arg } FieldMap
  => c
  -> Request
  -> a
  -> AffResolver
toObjectResolver c req = from >>> unsafeToObjectResolverRep c req (Proxy :: Proxy name)

unsafeToObjectResolverRep
  :: forall arg name ctrName c
   . IsSymbol name
  => HFoldlWithIndex (ToResolverProps c) FieldMap { | arg } FieldMap
  => c
  -> Request
  -> Proxy name
  -> (Constructor ctrName (Argument { | arg }))
  -> AffResolver
unsafeToObjectResolverRep c req _ (Constructor (Argument arg)) =
  Fields
    { fields: makeFields c req (reflectSymbol (Proxy :: Proxy name)) arg
    , typename: reflectSymbol (Proxy :: Proxy name)
    }

toUnionResolver
  :: forall a rep name
   . Generic a rep
  => GenericUnionResolver name rep
  => GqlRep a GUnion name
  => a
  -> AffResolver
toUnionResolver a = genericUnionResolver (Proxy :: Proxy name) $ from a

unsafeResolveNodeWith
  :: forall a
   . (a -> Json)
  -> Request
  -> a
  -> AffResolver
unsafeResolveNodeWith encode _ a = Node $ pure $ encode a

unsafeResolveNodeWithNoReq
  :: forall a
   . (a -> Json)
  -> a
  -> AffResolver
unsafeResolveNodeWithNoReq encode a = Node $ pure $ encode a

makeFields
  :: forall r c
   . HFoldlWithIndex (ToResolverProps c) FieldMap { | r } FieldMap
  => c
  -> Request
  -> String
  -> { | r }
  -> Map String (Field Error Aff)
makeFields c req typename r =
  unwrap $ ((hfoldlWithIndex (ToResolverProps c req :: ToResolverProps c) resolveTypename r) :: FieldMap)
  where
  resolveTypename :: FieldMap
  resolveTypename = FieldMap $ Map.singleton "__typename"
    { name: "__typename"
    , resolver: \_ -> unsafeResolveNodeNoReq typename
    }

data ToResolverProps c = ToResolverProps c Request

newtype FieldMap = FieldMap (Map String (Field Error Aff))

derive instance Newtype FieldMap _

instance
  ( IsSymbol sym
  , GetArgResolver c a
  ) =>
  FoldingWithIndex (ToResolverProps c) (Proxy sym) FieldMap a FieldMap where
  foldingWithIndex (ToResolverProps c req) prop (FieldMap fieldMap) a =
    FieldMap $ Map.insert name field fieldMap
    where
    name = reflectSymbol prop

    field :: Field Error Aff
    field =
      { name
      , resolver: getArgResolver c req a
      }

class GetArgResolver c a where
  getArgResolver
    :: c
    -> Request
    -> a
    -> { args :: Json }
    -> AffResolver

instance argResolverUnitFn :: ToResolver c a => GetArgResolver c (Unit -> a) where
  getArgResolver c req a = \_ -> toResolver c req (a unit)

else instance argResolverAllFn :: (DecodeArg a, ToResolver c b) => GetArgResolver c (a -> b) where
  getArgResolver c req fn { args } = case decodeArg args of
    Left err -> FailedResolver $ ResolverDecodeError err
    Right a -> toResolver c req $ fn a

else instance argResolverAny :: ToResolver c a => GetArgResolver c a where
  getArgResolver c req a = \_ -> toResolver c req a

class GenericUnionResolver :: forall k. k -> Type -> Constraint
class GenericUnionResolver sym rep where
  genericUnionResolver :: Proxy sym -> rep -> AffResolver

-- instance
--   ( GenericUnionResolver sym a
--   , GenericUnionResolver sym b
--   ) =>
--   GenericUnionResolver sym (Sum a b) where
--   genericUnionResolver sym (Inl a) = genericUnionResolver sym a
--   genericUnionResolver sym (Inr b) = genericUnionResolver sym b

-- instance
--   ( Applicative m
--   , IsSymbol fullName
--   , HFoldlWithIndex (ToResolverProps err m) FieldMap { | arg } FieldMap
--   , Append sym name fullName
--   ) =>
--   GenericUnionResolver sym (Constructor name (Argument { | arg })) where
--   genericUnionResolver _sym a = unsafeToObjectResolverRep (Proxy :: Proxy fullName) a
-- else instance
--   ( ToResolver c arg
--   ) =>
--   GenericUnionResolver _sym (Constructor name (Argument arg)) where
--   genericUnionResolver _sym (Constructor (Argument arg)) = toResolver arg