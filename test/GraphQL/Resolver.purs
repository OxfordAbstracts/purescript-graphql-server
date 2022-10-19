module Test.GraphQL.Resolver where

import Prelude

import Data.Filterable (filter)
import Data.Generic.Rep (class Generic)
import Effect.Aff (Aff)
import GraphQL.Resolver (rootResolver)
import GraphQL.Resolver.GqlIo (GqlIo(..))
import GraphQL.Resolver.JsonResolver (Resolver)
import GraphQL.Resolver.ToResolver (class ToResolver, objectResolver)
import GraphQL.Server.Schema.Introspection.GetType (class GetIType, genericGetIType)
import Test.Spec (Spec, describe, it)

spec :: Spec Unit
spec =
  describe "GraphQL.Resolver" do
    describe "rootResolver" do
      it "should resolve with a simple resolver" do
        pure unit

simpleResolver :: Resolver (GqlIo Aff)
simpleResolver =
  rootResolver
    { query: 
       { books: io [ book1, book2 ] 
       }
    , mutation: unit
    }


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

newtype Book = Book
  { name :: String
  , price :: Number
  , author :: Unit -> GqlIo Aff Author
  }

derive instance Generic Book _

instance ToResolver Book (GqlIo Aff) where
  toResolver a = objectResolver a

instance GetIType Book where
  getITypeImpl a = genericGetIType a

newtype Author = Author
  { name :: String
  , books :: { maxPrice :: Number } -> GqlIo Aff (Array Book)
  }

derive instance Generic Author _

instance ToResolver Author (GqlIo Aff) where
  toResolver a = objectResolver a

instance GetIType Author where
  getITypeImpl a = genericGetIType a
  
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

