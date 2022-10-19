module Test.GraphQL.Resolver.HandleOperation (spec) where

import Prelude

import Data.Argonaut (class EncodeJson, Json, encodeJson, jsonNull, stringify)
import Data.Either (either)
import Data.Filterable (filter)
import Data.Foldable (find)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff (Aff, error, throwError)
import Foreign.Object as Object
import GraphQL.Resolver (RootResolver, rootResolver)
import GraphQL.Resolver.GqlIo (GqlAff, GqlIo, io)
import GraphQL.Resolver.Gqlable (toAff)
import GraphQL.Resolver.HandleOperation (handleOperation)
import GraphQL.Resolver.ToResolver (class ToResolver, objectResolver, resolveGenericNode)
import GraphQL.Server.GqlResM as GqlM
import GraphQL.Server.HandleRequest (parseOperation)
import GraphQL.Server.Schema.Introspection.GetEnumValues (enumType)
import GraphQL.Server.Schema.Introspection.GetType (class GetIType, genericGetIType)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec =
  describe "GraphQL.Resolver.HandleOperation" do
    describe "handleOperation" do
      it "should resolve a simple query" do
        "query { books { id type } }" `shouldResolveTo`
          { books:
              [ { "id": 1, "type": "Paperback" }
              , { "id": 2, "type": "Ebook" }
              ]
          }
      it "should resolve a nested query with typenames" do
        """
        query { 
          books { 
            __typename
            id 
            name
            author { 
              __typename
              name 
            }
          } 
        }""" `shouldResolveTo`
          { books:
              [ { id: 1
                , name: "book name 1"
                , author:
                    { name: "author name"
                    , __typename: "Author"
                    }
                , __typename: "Book"
                }
              , { id: 2
                , name: "book name 2"
                , author:
                    { name: "author name"
                    , __typename: "Author"
                    }
                , __typename: "Book"
                }
              ]
          }
      it "should resolve a query with arguments" do
        """
        query { 
          books (maxPrice: 1) { 
            id 
            name
          } 
        }""" `shouldResolveTo`
          { books:
              [ { id: 1
                , name: "book name 1"
                }
              ]
          }
      it "should resolve a schema introspection query" do
        """
        query { 
          __schema { 
            queryType { 
              name
              fields {
                name
                args {
                  name
                  type {
                    name
                    ofType {
                      name
                    }
                  }
                }
              }
            }
          }
        }""" `shouldResolveTo`
          { "__schema":
              { "queryType":
                  { "name": "QueryRoot"
                  , "fields":
                      [ { "name": "book"
                        , "args": encodeJson
                            [ { "name": "id"
                              , "type":
                                  { "name": jsonNull
                                  , "ofType": { "name": "Int" }
                                  }
                              }
                            ]
                        }
                      , { "name": "books"
                        , "args": encodeJson
                            [ { "name": "maxPrice"
                              , "type":
                                  { "name": "Float"
                                  , "ofType": jsonNull
                                  }
                              }
                            ]
                        }
                      ]
                  }
              }
          }
      it "should resolve a type introspection query" do
        """
        query { 
          __type (name: "Book") {
            name
            fields {
              name
              type {
                name
                kind
                ofType {
                  name
                  kind
                }
              }
            }
          } 
        }""" `shouldResolveTo`
          { "__type":
              { "name": "Book"
              , "fields":
                  [ { "name": "author"
                    , "type": encodeJson
                        { "name": jsonNull
                        , "kind": "NON_NULL"
                        , "ofType": { "name": "Author", "kind": "OBJECT" }
                        }
                    }
                  , { "name": "id"
                    , "type": encodeJson
                        { "name": jsonNull
                        , "kind": "NON_NULL"
                        , "ofType": { "name": "Int", "kind": "SCALAR" }
                        }
                    }
                  , { "name": "name"
                    , "type": encodeJson
                        { "name": jsonNull
                        , "kind": "NON_NULL"
                        , "ofType": { "name": "String", "kind": "SCALAR" }
                        }
                    }
                  , { "name": "price"
                    , "type": encodeJson
                        { "name": jsonNull
                        , "kind": "NON_NULL"
                        , "ofType": { "name": "Float", "kind": "SCALAR" }
                        }
                    }
                  , { "name": "type"
                    , "type": encodeJson
                        { "name": "BookType"
                        , "kind": "ENUM"
                        , "ofType": jsonNull
                        }
                    }
                  ]
              }
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
  op <- GqlM.toAff' $ parseOperation Nothing query
  eit <- toAff $ handleOperation simpleResolver op Object.empty
  res <- either (throwError <<< error <<< show) pure eit
  pure res.data

simpleResolver :: RootResolver GqlAff
simpleResolver =
  rootResolver
    { query:
        { books: \(args :: { maxPrice :: Maybe Number }) -> io $ [ book1, book2 ]
            # filter (\(Book b) -> maybe true ((<=) b.price) args.maxPrice)
        , book: \(args :: { id :: Int }) ->
            find (\(Book b) -> b.id == args.id) [ book1, book2 ]
        }
    , mutation:
        { action1: io "action1"
        }
    }

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

newtype Book = Book
  { name :: GqlIo Aff String
  , id :: Int
  , price :: Number
  , type :: Maybe BookType
  , author :: Unit -> GqlIo Aff Author
  }

derive instance Generic Book _

instance ToResolver Book GqlAff where
  toResolver a = objectResolver a

instance GetIType Book where
  getITypeImpl a = genericGetIType a

newtype Author = Author
  { name :: String
  , books :: { maxPrice :: Number } -> GqlIo Aff (Array Book)
  }

derive instance Generic Author _

instance ToResolver Author GqlAff where
  toResolver a = objectResolver a

instance GetIType Author where
  getITypeImpl a = genericGetIType a

data BookType = Paperback | Hardback | Ebook

derive instance Generic BookType _

instance ToResolver BookType GqlAff where
  toResolver a = resolveGenericNode a

instance GetIType BookType where
  getITypeImpl a = enumType "BookType" a