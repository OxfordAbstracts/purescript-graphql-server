module Test.GraphQL.Resolver.HandleOperation (spec) where

import Prelude

import Data.Argonaut (class EncodeJson, Json, encodeJson, jsonNull)
import Data.DateTime (DateTime)
import Data.Either (either)
import Data.Filterable (filter)
import Data.Foldable (find)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff (Aff, Error, error, throwError)
import Foreign.Object as Object
import GraphQL.Resolver (RootResolver, rootResolver)
import GraphQL.Resolver.EvalGql (evalGql)
import GraphQL.Resolver.GqlIo (GqlAff, GqlIo, gPure)
import GraphQL.Resolver.HandleOperation (handleOperation)
import GraphQL.Server.Gql (class Gql, enum, object, scalar, union)
import GraphQL.Server.GqlResM as GqlM
import GraphQL.Server.HandleRequest (parseOperation)
import HTTPure (Request)
import Test.GraphQL.E2E.Util (JsonTest(..))
import Test.Spec (Spec, describe, describeOnly, it, itOnly)
import Test.Spec.Assertions (shouldEqual)
import Unsafe.Coerce (unsafeCoerce)

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
      it "should resolve a query with aliases" do
        """
        query { 
          low_price_books: books (maxPrice: 1) { 
            id 
            name
          } 
        }""" `shouldResolveTo`
          { low_price_books:
              [ { id: 1
                , name: "book name 1"
                }
              ]
          }

      it "should resolve a query with variables" do
        { query:
            """query getBook($maxPrice: Int) { 
            books(maxPrice: $maxPrice) { 
              id 
              name
            } 
          }"""
        , vars: Object.fromHomogeneous
            { maxPrice: encodeJson 1 }

        }
          `shouldResolveToWithVars`
            { books:
                [ { id: 1
                  , name: "book name 1"
                  }
                ]
            }

      it "should resolve a query with union types" do
        """
        query { 
          books { 
            id 
            packaging {
              __typename
              ... on Packaging_Boxed {
                note
              }
              ... on Packaging_GiftWrapped {
                colour
              }
            }
          }
        }""" `shouldResolveTo`
          { "books":
              [ { "id": 1
                , "packaging": encodeJson
                    { "__typename": "Packaging_Boxed"
                    , "note": "Custom note"
                    }
                }
              , { "id": 2
                , "packaging": encodeJson
                    { "__typename": "Packaging_GiftWrapped"
                    , "colour": "Blue"
                    }
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
                  , { "name": "created_at"
                    , "type": encodeJson
                        { "name": "DateTime"
                        , "kind": "SCALAR"
                        , "ofType": jsonNull
                        }
                    }
                  , { "name": "custom_scalar"
                    , "type": encodeJson
                        { "name": jsonNull
                        , "kind": "NON_NULL"
                        , "ofType": { "name": "CustomScalar", "kind": "SCALAR" }
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
  JsonTest res `shouldEqual` JsonTest (encodeJson expected)

shouldResolveToWithVars :: forall a. EncodeJson a => { query :: String, vars :: Object.Object Json } -> a -> Aff Unit
shouldResolveToWithVars { vars, query } expected = do
  res <- resolveAsJsonWithVars vars query
  JsonTest res `shouldEqual` JsonTest (encodeJson expected)

resolveAsJson :: String -> Aff Json
resolveAsJson = resolveAsJsonWithVars Object.empty

resolveAsJsonWithVars :: Object.Object Json -> String -> Aff Json
resolveAsJsonWithVars vars query = do
  op <- GqlM.toAff' $ parseOperation Nothing query
  eit <- evalGql mockRequest $ handleOperation simpleResolver mockRequest op vars
  res <- either (throwError <<< error <<< show) pure eit
  pure res.data

simpleResolver :: RootResolver Error GqlAff
simpleResolver =
  rootResolver
    { query:
        { books: \(args :: { maxPrice :: Maybe Number }) -> gPure $ [ book1, book2 ]
            # filter (\(Book b) -> maybe true ((<=) b.price) args.maxPrice)
        , book: \(args :: { id :: Int }) ->
            find (\(Book b) -> b.id == args.id) [ book1, book2 ]
        }
    , mutation:
        { action1: gPure "action1"
        }
    }

book1 :: Book
book1 = Book
  { name: pure "book name 1"
  , id: 1
  , price: 1.0
  , type: Just Paperback
  , packaging: Just $ Boxed { note: "Custom note" }
  , author: \_ -> pure author
  , created_at: Just bottom
  , custom_scalar: CustomScalar "1" 1
  }

book2 :: Book
book2 = Book
  { name: pure "book name 2"
  , id: 2
  , price: 2.0
  , type: Just Ebook
  , packaging: Just $ GiftWrapped { colour: "Blue", withCard: true }
  , author: \_ -> pure author
  , created_at: Just top
  , custom_scalar: CustomScalar "2" 2
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
  , created_at :: Maybe DateTime
  , custom_scalar :: CustomScalar
  }

derive instance Generic Book _

instance Gql Book where
  gql _ = object unit

newtype Author = Author
  { name :: String
  , books :: { maxPrice :: Number } -> GqlIo Aff (Array Book)
  }

derive instance Generic Author _

instance Gql Author where
  gql _ = object unit

data BookType = Paperback | Hardback | Ebook

derive instance Generic BookType _

instance Gql BookType where
  gql = enum "BookType"

data Packaging
  = GiftWrapped
      { colour :: String
      , withCard :: Boolean
      }
  | Boxed
      { note :: String
      }
  | CustomPacking String

derive instance Generic Packaging _

instance Gql Packaging where
  gql = union "Packaging"

data CustomScalar = CustomScalar String Int

instance Gql CustomScalar where
  gql _ = scalar encode "CustomScalar"
    where
    encode (CustomScalar s i) = encodeJson { s, i }

mockRequest :: Request
mockRequest = unsafeCoerce unit