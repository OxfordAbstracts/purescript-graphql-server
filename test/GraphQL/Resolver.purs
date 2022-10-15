module Test.GraphQL.Resolver where

import Prelude

import Data.Either (Either(..))
import Data.Filterable (filter)
import Data.Generic.Rep (class Generic, repOf)
import Effect.Aff (Aff)
import GraphQL.Resolver (rootResolver)
import GraphQL.Resolver.GqlIo (GqlIo(..))
import GraphQL.Resolver.JsonResolver (Resolver)
import GraphQL.Resolver.Root (GqlRoot)
import GraphQL.Resolver.ToResolver (class ToResolver, genericResolver)
import GraphQL.Server.Schema.GetDocument (class GetDocument)
import GraphQL.Server.Schema.Introspection.TypeName (class GqlTypeName)
import Partial.Unsafe (unsafeCrashWith)
import Test.Spec (Spec, describe, it)
import Type.Proxy (Proxy(..))

spec :: Spec Unit
spec =
  describe "GraphQL.Resolver" do
    describe "rootResolver" do
      it "should resolve with a simple resolver" do
        pure unit

-- simpleResolver :: Resolver (GqlIo Aff)
-- simpleResolver =
--   unsafeRootResolver
--     { query: simpleQuery
--     , mutation: unit
--     }

simpleQuery :: SimpleQuery
simpleQuery = SimpleQuery
  { books: io [ book1, book2 ] }

book1 :: Book
book1 = Book
  { name: "book name 1"
  , price: 1.0
  , author: \_ -> io author
  }

book2 :: Book
book2 = Book
  { name: "book name 2"
  , price: 1.0
  , author: \_ -> io author
  }

author :: Author
author = Author
  { name: "author name"
  , books: \{ maxPrice } -> io $
      [ book1, book2 ] # filter \(Book b) -> b.price <= maxPrice
  }

newtype SimpleQuery = SimpleQuery { books :: GqlIo Aff (Array Book) }

instance GqlTypeName SimpleQuery "SimpleQuery"

derive instance Generic SimpleQuery _

instance ToResolver SimpleQuery (GqlIo Aff) where
  toResolver a = genericResolver a

newtype Book = Book
  { name :: String
  , price :: Number
  , author :: Unit -> GqlIo Aff Author
  }

instance GqlTypeName Book "Book"

derive instance Generic Book _

instance ToResolver Book (GqlIo Aff) where
  toResolver a = genericResolver a

newtype Author = Author
  { name :: String
  , books :: { maxPrice :: Number } -> GqlIo Aff (Array Book)
  }

instance GqlTypeName Author "Author"

derive instance Generic Author _

instance ToResolver Author (GqlIo Aff) where
  toResolver a = genericResolver a

io :: forall a. a -> GqlIo Aff a
io = GqlIo <<< (pure :: _ -> Aff _)

-- unsafeRootResolver
--   :: forall query mutation m
--    . Applicative m
--   => ToResolver (GqlRoot query mutation) m
--   => GetDocument (GqlRoot query mutation)
--   => { query :: query, mutation :: mutation }
--   -> Resolver m
-- unsafeRootResolver = rootResolver >>> case _ of
--   Left err -> unsafeCrashWith err
--   Right a -> a

