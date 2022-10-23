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
import GraphQL.GqlRep (class GqlRep, GEnum, GObject, GUnion)
import GraphQL.Resolver (RootResolver, rootResolver)
import GraphQL.Resolver.GqlIo (GqlAff, GqlIo, io)
import GraphQL.Resolver.Gqlable (toAff)
import GraphQL.Resolver.HandleOperation (handleOperation)
import GraphQL.Resolver.ToResolver (class ToResolver, toEnumResolver, toObjectResolver, toUnionResolver)
import GraphQL.Server.GqlResM as GqlM
import GraphQL.Server.HandleRequest (parseOperation)
import GraphQL.Server.Schema.Introspection.GetType (class GetGqlType, getEnumType, getObjectType, getUnionType)
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
                  , { "name": "packaging"
                    , "type": encodeJson
                        { "name": "Packaging"
                        , "kind": "UNION"
                        , "ofType": jsonNull
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
  , packaging: Just $ PackagingBoxed $ Boxed { note: "Custom note" }
  , author: \_ -> pure author
  }

book2 :: Book
book2 = Book
  { name: pure "book name 2"
  , id: 2
  , price: 2.0
  , type: Just Ebook
  , packaging: Just $ PackagingBoxed $ Boxed { note: "Custom note" }
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
  , packaging :: Maybe Packaging
  , author :: Unit -> GqlIo Aff Author
  }

derive instance Generic Book _

instance GqlRep Book GObject "Book"

instance ToResolver Book GqlAff where
  toResolver a = toObjectResolver a

instance GetGqlType Book where
  getType a = getObjectType a

newtype Author = Author
  { name :: String
  , books :: { maxPrice :: Number } -> GqlIo Aff (Array Book)
  }

derive instance Generic Author _

instance GqlRep Author GObject "Author"

instance ToResolver Author GqlAff where
  toResolver a = toObjectResolver a

instance GetGqlType Author where
  getType a = getObjectType a

data BookType = Paperback | Hardback | Ebook

instance GqlRep BookType GEnum "BookType"

derive instance Generic BookType _

instance ToResolver BookType GqlAff where
  toResolver a = toEnumResolver a

instance GetGqlType BookType where
  getType a = getEnumType a

data Packaging
  = PackagingGiftWrapped GiftWrapped
  | PackagingBoxed Boxed

derive instance Generic Packaging _

instance GqlRep Packaging GUnion "Packaging"

instance ToResolver Packaging GqlAff where
  toResolver a = toUnionResolver a

instance GetGqlType Packaging where
  getType a = getUnionType a

newtype GiftWrapped = GiftWrapped
  { colour :: String
  }

derive instance Generic GiftWrapped _

instance GqlRep GiftWrapped GObject "GiftWrapped"

instance ToResolver GiftWrapped GqlAff where
  toResolver a = toObjectResolver a

instance GetGqlType GiftWrapped where
  getType a = getObjectType a

newtype Boxed = Boxed
  { note :: String
  }

derive instance Generic Boxed _

instance GqlRep Boxed GObject "Boxed"

instance ToResolver Boxed GqlAff where
  toResolver a = toObjectResolver a

instance GetGqlType Boxed where
  getType a = getObjectType a