module Test.GraphQL.Resolver.HandleOperation (spec) where

import Prelude

import Data.Argonaut (class EncodeJson, Json, encodeJson, stringify)
import Data.Either (either)
import Data.Filterable (filter)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect.Aff (Aff, error, throwError)
import Foreign.Object as Object
import GraphQL.Resolver (RootResolver, rootResolver)
import GraphQL.Resolver.GqlIo (GqlIo(..))
import GraphQL.Resolver.HandleOperation (handleOperation)
import GraphQL.Resolver.ToResolver (class ToResolver, objectResolver)
import GraphQL.Server.GqlResM (toAff')
import GraphQL.Server.HandleRequest (parseOperation)
import GraphQL.Server.Schema.Introspection.GetType (class GetIType, genericGetIType)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec =
  describe "GraphQL.Resolver.HandleOperation" do
    describe "handleOperation" do
      it "should resolve with a simple resolver" do
        "query { books { id } }" `shouldResolveTo`
          { books: [ { id: 1 }, { id: 2 } ]
          }
      it "should resolve with a nested resolver" do
        """
        query { 
          books { 
            id 
            name
            author { 
              name 
            }
          } 
        }""" `shouldResolveTo`
          { books:
              [ { id: 1
                , name: "book name 1"
                , author: { name: "author name" }
                }
              , { id: 2
                , name: "book name 2"
                , author: { name: "author name" }
                }
              ]
          }

shouldResolveTo :: forall a. EncodeJson a => String -> a -> Aff Unit
shouldResolveTo query expected = do
  res <- resolveAsJson query
  JsonShow res `shouldEqual` JsonShow (encodeJson expected)

newtype JsonShow = JsonShow Json

derive newtype instance Eq JsonShow

instance Show JsonShow where
  show (JsonShow j) = stringify j

resolveAsJson :: String -> Aff Json
resolveAsJson query = do
  op <- toAff' $ parseOperation Nothing query
  eit <- (unwrap $ handleOperation simpleResolver op Object.empty)
  res <- either (throwError <<< error <<< show) pure eit
  pure res.data

simpleResolver :: RootResolver (GqlIo Aff)
simpleResolver =
  rootResolver
    { query:
        { books: GqlIo $ pure [ book1, book2 ]
        }
    , mutation: {}
    }

book1 :: Book
book1 = Book
  { name: pure "book name 1"
  , id: 1
  , price: 1.0
  , author: \_ -> pure author
  }

book2 :: Book
book2 = Book
  { name: pure "book name 2"
  , id: 2
  , price: 2.0
  , author: \_ -> pure author
  }

author :: Author
author = Author
  { name: "author name"
  , books: \{ maxPrice } -> pure $
      [ book1, book2 ] # filter \(Book b) -> b.price <= maxPrice
  }

newtype Book = Book
  { name :: GqlIo Aff String
  , id :: Int
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
