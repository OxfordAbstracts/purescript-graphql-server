module GraphQL.GqlRep where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson)
import Data.Argonaut.Encode.Generic (class EncodeLiteral)
import Data.Either (Either(..))
import Data.Filterable (filter)
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), from)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Effect.Aff (Aff)
import GraphQL.Resolver.GqlIo (GqlIo)
import GraphQL.Resolver.JsonResolver (Fields, Field(..), Resolver(..))
import GraphQL.Resolver.ToResolver (resolveEnum, resolveNode)
import GraphQL.Server.GqlError (ResolverError(..))
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Type.Proxy (Proxy(..))

-- | How a Purescript type is represented in GraphQL
-- | This then defines its serialization and introspection type
class GqlRep :: forall k1 k2. k1 -> k2 -> Symbol -> Constraint
class
  GqlRep a gqlType name
  | a -> name
  -- , name -> a
  , a -> gqlType
  , name -> gqlType

-- , name -> name
-- , gqlType -> gqlType

instance GqlRep Boolean GScalar "Boolean"

instance GqlRep Int GScalar "Int"

instance GqlRep Number GScalar "Float"

instance GqlRep String GScalar "String"

instance GqlRep a gql name => GqlRep (GqlIo m a) gql name

data GEnum

data GScalar

data GObject

data GAsync

class Res :: forall k1. Type -> k1 -> Symbol -> (Type -> Type) -> Constraint
class
  GqlRep a gqlType name <=
  Res a gqlType name m
  | a -> name
  , gqlType -> name

  -- , name -> a
  -- , name -> gqlType
  , m -> m
  where
  resImpl :: a -> Proxy name -> Proxy gqlType -> Resolver m

instance
  ( Res a gql name (GqlIo m)
  , Functor m
  ) =>
  Res (GqlIo m a) gql name (GqlIo m) where
  resImpl = resolveAsync
else
-- instance
--   ( Res a gql name (GqlIo m)
--   , Functor m
--   ) =>
--   Res a gql name (GqlIo m) where
--   resImpl = ?resolveAsync
-- else
instance
  ( Applicative m
  , Generic a (Constructor name (Argument { | arg }))
  , IsSymbol name
  , HFoldlWithIndex (ResProps m) (FieldMap m) { | arg } (FieldMap m)
  , GqlRep a GObject name
  -- , Res a GObject name m
  ) =>
  Res a GObject name m where
  resImpl a _ _ = objectResolver a
else instance
  ( Applicative m
  , EncodeLiteral rep
  , Generic a rep
  , GqlRep a GEnum name
  ) =>
  Res a GEnum name m where
  resImpl a _ _ = resolveEnum a

else instance
  ( Applicative m
  , EncodeJson a
  , GqlRep a GScalar name
  ) =>
  Res a GScalar name m where
  resImpl a _ _ = resolveNode a

-- a _ _ = resImpl (Proxy :: Proxy name) (Proxy :: Proxy gql) ?D

resolveAsync :: forall m a gql name. Functor m => Res a gql name m => m a -> Proxy name -> Proxy gql -> Resolver m
resolveAsync a _ _ = ResolveAsync $ res <$> a

objectResolver
  :: forall m a arg name
   . Applicative m
  => Generic a (Constructor name (Argument { | arg }))
  => IsSymbol name
  => HFoldlWithIndex (ResProps m) (FieldMap m) { | arg } (FieldMap m)
  => a
  -> Resolver m
objectResolver = from >>> \(Constructor (Argument arg)) ->
  Fields
    { fields: makeFields (reflectSymbol (Proxy :: Proxy name)) arg
    , typename: reflectSymbol (Proxy :: Proxy name)
    }

makeFields
  :: forall r m
   . HFoldlWithIndex (ResProps m) (FieldMap m) { | r } (FieldMap m)
  => Applicative m
  => String
  -> { | r }
  -> Map String (Field m)
makeFields typename r =
  unwrap $ ((hfoldlWithIndex (ResProps :: ResProps m) resolveTypename r) :: FieldMap m)
  where
  resolveTypename :: FieldMap m
  resolveTypename = FieldMap $ Map.singleton "__typename"
    { name: "__typename"
    , resolver: \_ -> resolveNode typename
    }

data ResProps :: forall k. k -> Type
data ResProps m = ResProps

newtype FieldMap m = FieldMap (Map String (Field m))

derive instance Newtype (FieldMap m) _

instance
  ( IsSymbol sym
  , GetArgResolver a m
  ) =>
  FoldingWithIndex (ResProps m) (Proxy sym) (FieldMap m) a (FieldMap m) where
  foldingWithIndex ResProps prop (FieldMap fieldMap) a =
    FieldMap $ Map.insert name field fieldMap
    where
    name = reflectSymbol prop

    field :: Field m
    field =
      { name
      , resolver: getArgResolver a
      }

class GetArgResolver a m | m -> m where
  getArgResolver
    :: a
    -> { args :: Json }
    -> Resolver m

instance argResolverUnitFn :: Res a gql name m => GetArgResolver (Unit -> a) m where
  getArgResolver a = \_ -> res (a unit)

else instance argResolverAllFn :: (DecodeJson a, Res b gql name m) => GetArgResolver (a -> b) m where
  getArgResolver fn { args } = case decodeJson args of
    Left err -> FailedResolver $ ResolverDecodeError err
    Right a -> res $ fn a

else instance argResolverAny :: Res a gql name m => GetArgResolver a m where
  getArgResolver a = \_ -> res a

res :: forall a gqlType name m. Res a gqlType name m => GqlRep a gqlType name => a -> Resolver m
res a = resImpl a (Proxy :: Proxy name) (Proxy :: Proxy gqlType)

t0 :: forall m. Applicative m => Resolver m
t0 = res "1"

t1 :: forall m. Applicative m => Resolver m
t1 = res 1

t2 :: forall m. Applicative m => Resolver m
t2 = res Paperback

t3 :: Resolver (GqlIo Aff)
t3 = res book1

newtype Book = Book
  { name :: GqlIo Aff String
  , id :: Int
  , price :: Number
  , type :: Maybe BookType
  , author :: Unit -> GqlIo Aff Author
  }

derive instance Generic Book _

instance GqlRep Book GObject "Book"

newtype Author = Author
  { name :: String
  , books :: { maxPrice :: Number } -> GqlIo Aff (Array Book)
  }

derive instance Generic Author _

instance GqlRep Author GObject "Author"

data BookType = Paperback | Hardback | Ebook

derive instance Generic BookType _

instance GqlRep BookType GEnum "BookType"

book1 :: Book
book1 = Book
  { name: pure "book name 1"
  , id: 1
  , price: 1.0
  , type: Just Paperback
  , author: \_ -> pure author
  }

book2 :: Book
book2 = Book
  { name: pure "book name 2"
  , id: 2
  , price: 2.0
  , type: Just Ebook
  , author: \_ -> pure author
  }

author :: Author
author = Author
  { name: "author name"
  , books: \{ maxPrice } -> pure $
      [ book1, book2 ] # filter \(Book b) -> b.price <= maxPrice
  }