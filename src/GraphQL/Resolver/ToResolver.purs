module GraphQL.Resolver.ToResolver
  ( class ToResolver
  , class GetArgResolver
  , class ToResolverCustom
  , toResolverCustom
  , ToResolverProps(..)
  , FieldMap(..)
  , getArgResolver
  , toResolver
  , resolveNode
  ) where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), from)
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Typelevel.Num (class IsZero, class Nat, class Pos, class Succ, D0)
import GraphQL.Resolver.GqlIo (GqlIo)
import GraphQL.Resolver.JsonResolver (Field, Resolver(..))
import GraphQL.Resolver.JsonResolver as JsonResolver
import GraphQL.Resolver.Root (GqlRoot(..))
import GraphQL.Server.GqlError (ResolverError(..))
import GraphQL.Server.Schema.Introspection.Types (IEnumValue(..), IType(..), ITypeKind, IType_T)
import GraphQL.Server.Schema.Introspection.Types.DirectiveLocation (IDirectiveLocation)
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Type.Proxy (Proxy(..))

class Nat n <= ToResolver n a m where
  toResolver :: Proxy n -> a -> JsonResolver.Resolver m

resolveNode :: forall x m a. Applicative m => EncodeJson a => x -> a -> Resolver m
resolveNode _n a = Node $ pure $ encodeJson a

-- resolveAsync :: forall f a. Functor f => ToResolver a f => f a -> Resolver f
resolveAsync :: forall m n a. Functor m => ToResolver n a m => Proxy n -> m a -> Resolver m
resolveAsync n a = ResolveAsync $ toResolver n <$> a

instance (ToResolver n a (GqlIo m), Applicative m) => ToResolver n (GqlIo m a) (GqlIo m) where
  toResolver a = resolveAsync a

else instance (Applicative m, Nat n) => ToResolver n Boolean m where
  toResolver a = resolveNode a

else instance (Applicative m, Nat n) => ToResolver n Int m where
  toResolver a = resolveNode a

else instance (Applicative m, Nat n) => ToResolver n Number m where
  toResolver a = resolveNode a

else instance (Applicative m, Nat n) => ToResolver n String m where
  toResolver a = resolveNode a

else instance (Applicative m, Nat n) => ToResolver n Json m where
  toResolver a = resolveNode a

else instance (Applicative m, Nat n) => ToResolver n Unit m where
  toResolver a = resolveNode a

else instance (Applicative m, Nat n) => ToResolver n Void m where
  toResolver a = resolveNode a

else instance (Applicative m, Nat n) => ToResolver n IDirectiveLocation m where
  toResolver a = resolveNode a

else instance (Applicative m, Nat n) => ToResolver n ITypeKind m where
  toResolver a = resolveNode a

else instance (Applicative m, ToResolver n a m) => ToResolver n (List a) m where
  toResolver n a = ListResolver $ toResolver n <$> a

else instance (Applicative m, ToResolver n a m) => ToResolver n (Maybe a) m where
  toResolver n a = NullableResolver $ toResolver n <$> a

else instance (Applicative m, ToResolver n a m) => ToResolver n (Array a) m where
  toResolver n a = ListResolver $ map (toResolver n) $ List.fromFoldable a

else instance ToResolver n a m => ToResolver n (Unit -> a) m where
  toResolver n a = toResolver n $ a unit

else instance
  Applicative m =>
  ToResolver D0 a m where
  toResolver _ _ = FailedResolver MaximumDepthExceeded

else instance
  ( Applicative m
  , Nat n
  , Succ pred n
  , HFoldlWithIndex (ToResolverProps pred m) (FieldMap m) ({ query :: q, mutation :: mut }) (FieldMap m)
  , IsSymbol "root"
  ) =>
  ToResolver n (GqlRoot q mut) m where
  toResolver _n (GqlRoot root) = Fields
    { fields: makeFields (Proxy :: Proxy pred) (reflectSymbol (Proxy :: Proxy "root")) root
    , typename: "root"
    }

else instance
  ( Generic r rep
  , ToResolverCustom n rep m
  ) =>
  ToResolver n r m where
  toResolver n a = toResolverCustom n $ from a

class Nat n <= ToResolverCustom n a m where
  toResolverCustom :: Proxy n -> a -> JsonResolver.Resolver m

instance
  ( Applicative m
  , Nat nat
  , Succ pred nat
  , IsSymbol name
  , HFoldlWithIndex (ToResolverProps pred m) (FieldMap m) { | arg } (FieldMap m)
  ) =>
  ToResolverCustom nat (Constructor name (Argument { | arg })) m where
  toResolverCustom _n (Constructor (Argument arg)) =
    Fields
      { fields: makeFields (Proxy :: Proxy pred) (reflectSymbol (Proxy :: Proxy name)) arg
      , typename: reflectSymbol (Proxy :: Proxy name)
      }

makeFields
  :: forall r m n
   . HFoldlWithIndex (ToResolverProps n m) (FieldMap m) { | r } (FieldMap m)
  => Applicative m
  => Proxy n
  -> String
  -> { | r }
  -> Map String (Field m)
makeFields proxy typename r =
  unwrap $ ((hfoldlWithIndex (ToResolverProps :: ToResolverProps n m) resolveTypename r) :: FieldMap m)
  where
  resolveTypename :: FieldMap m
  resolveTypename = FieldMap $ Map.singleton "__typename"
    { name: "__typename"
    , resolver: \_ -> resolveNode proxy typename
    }

data ToResolverProps :: forall k1 k2. k1 -> k2 -> Type
data ToResolverProps n m = ToResolverProps

newtype FieldMap m = FieldMap (Map String (Field m))

derive instance Newtype (FieldMap m) _

instance
  ( IsSymbol sym
  , GetArgResolver n a m
  ) =>
  FoldingWithIndex (ToResolverProps n m) (Proxy sym) (FieldMap m) a (FieldMap m) where
  foldingWithIndex ToResolverProps prop (FieldMap fieldMap) a =
    FieldMap $ Map.insert name field fieldMap
    where
    name = reflectSymbol prop

    field :: Field m
    field =
      { name
      , resolver: getArgResolver (Proxy :: Proxy n) a
      }

class Nat n <= GetArgResolver n a m where
  getArgResolver
    :: Proxy n
    -> a
    -> { args :: Json }
    -> JsonResolver.Resolver m

instance argResolverUnitFn :: ToResolver n a m => GetArgResolver n (Unit -> a) m where
  getArgResolver n a = \_ -> toResolver n (a unit)

else instance argResolverAllFn :: (DecodeJson a, ToResolver n b m) => GetArgResolver n (a -> b) m where
  getArgResolver n fn { args } = case decodeJson args of
    Left err -> FailedResolver $ ResolverDecodeError err
    Right a -> toResolver n $ fn a

else instance argResolverAny :: ToResolver n a m => GetArgResolver n a m where
  getArgResolver n a = \_ -> toResolver n a
