module Test.GraphQL.Server.Resolver.JsonResolver (spec) where

import Prelude

import Data.Argonaut (class EncodeJson, encodeJson)
import Data.Either (Either(..))
import Data.Filterable (filter)
import Data.Foldable (class Foldable)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.Map as Map
import Data.Maybe (Maybe, maybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, Error)
import Effect.Exception (Error, message)
import GraphQL.Resolver.EvalGql (evalGql)
import GraphQL.Resolver.GqlIo (GqlEffect, GqlIo(..), GqlAff, gPure)
import GraphQL.Resolver.InstanceCache as IC
import GraphQL.Resolver.JsonResolver (Field, Resolver(..), resolveQueryString)
import GraphQL.Resolver.Result (Result(..))
import GraphQL.Server.Gql (class Gql, object, toResolver)
import GraphQL.Server.GqlError (GqlError, FailedToResolve(..))
import HTTPure (Request)
import Test.GraphQL.Server.Resolver.ToResolver (gqlObj, leaf)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Unsafe.Coerce (unsafeCoerce)

spec :: Spec Unit
spec =
  describe "GraphQL.Server.Resolver.JsonResolver" do
    describe "resolveQueryString" do
      it "should resolve a flat query string" do
        let

          query = "{ top_level_1 top_level_2 top_level_3 }"

          -- expected :: Result Error
          expected = ResultObject
            $ Tuple "top_level_1" (ResultLeaf $ encodeJson "top_val_1")
                : Tuple "top_level_2" (ResultLeaf $ encodeJson [ 1 ])
                : Tuple "top_level_3" (ResultLeaf $ encodeJson { "l2": 2, "l1": "v1" })
                : Nil

        actual <- resolveTestQuery resolver query
        actual `shouldEqual` Right expected
      it "should resolve a nested query string" do
        let
          query = "{ top_level_nested_1 {c1 c2} }"

          -- expected :: Result Error
          expected = ResultObject
            $ pure
            $ Tuple "top_level_nested_1"
            $ ResultObject
            $ Tuple "c1" (ResultLeaf $ encodeJson "v11")
                : Tuple "c2" (ResultLeaf $ encodeJson "v12")
                : Nil

        actual <- resolveTestQuery resolver query
        actual `shouldEqual` Right expected

      it "should resolve a recursive resolver constucted using `toResolver` " do
        res <- evalGql mockRequest $ resolveTestQuery  booksResolver
          """{ books (maxPrice: 7) { 
            title 
            author { 
              name 
              books { 
                title 
              }
            }
            woops
          } 
        }"""
        res `shouldEqual`
          ( Right
              $ ResultObject
                  ( ( Tuple "books"
                        ( ResultList
                            ( ( ResultObject
                                  ( (Tuple "title" (leaf "Consider Phlebas"))
                                      :
                                        ( Tuple "author"
                                            ( ResultObject
                                                ( ( Tuple "name"
                                                      ( leaf "Iain M. Banks"
                                                      )
                                                  )
                                                    :
                                                      ( Tuple "books"
                                                          ( ResultList
                                                              ( (ResultObject ((Tuple "title" (leaf "State of the Art")) : Nil))
                                                                  : (ResultObject ((Tuple "title" (leaf "Consider Phlebas")) : Nil))
                                                                  : Nil
                                                              )
                                                          )
                                                      )
                                                    : Nil
                                                )
                                            )
                                        )
                                      : (Tuple "woops" (ResultError FieldNotFound))
                                      : Nil
                                  )
                              ) : Nil
                            )
                        )
                    ) : Nil
                  )

          )

resolver :: forall err m. Applicative m => Resolver err m
resolver = Fields
  { typename: "name"
  , fields:
      mkFieldMap
        [ { name: "top_level_1"
          , resolver: resolveNode "top_val_1"
          }
        , { name: "top_level_2"
          , resolver: resolveNode [ 1 ]
          }
        , { name: "top_level_3"
          , resolver: resolveNode { l1: "v1", l2: 2 }
          }
        , { name: "top_level_nested_1"
          , resolver: \_ -> Fields
              { typename: "name"
              , fields: mkFieldMap
                  [ { name: "c1"
                    , resolver: resolveNode "v11"
                    }
                  , { name: "c2"
                    , resolver: resolveNode "v12"
                    }
                  ]
              }
          }
        ]
  }

resolveNode ∷ ∀ (args ∷ Type) (m ∷ Type -> Type) (a ∷ Type) err. Applicative m ⇒ EncodeJson a ⇒ a → args → Resolver err m
resolveNode a _ = Node $ pure $ encodeJson a

mkFieldMap
  :: forall err f m
   . Foldable f
  => Functor f
  => f (Field err m)
  -> Map.Map String (Field err m)
mkFieldMap = Map.fromFoldable <<< map (\f -> Tuple f.name f)



booksResolver ::  Resolver Error GqlAff
booksResolver =
  flip toResolver mockRequest $ TopLevel
    { books
    }
  where
  books :: _ -> GqlIo Aff _
  books = \(opts :: { maxPrice :: Maybe Number }) -> do
    gPure $
      filter (\(Book b) -> maybe true (b.price <= _) opts.maxPrice)
        books_

  author :: (Author)
  author = Author
    { name: "Iain M. Banks"
    , bio: io "This is some stuff about the author"
    , books: \_ -> books_
    }

  books_ =
    [ Book
        { title: "State of the Art"
        , price: 9.99
        , author: \_ -> gPure author
        }
    , Book
        { title: "Consider Phlebas"
        , price: 5.99
        , author: \_ -> gPure author
        }
    ]

newtype TopLevel = TopLevel
  { books :: { maxPrice :: Maybe Number } -> GqlIo Aff (Array Book)
  }

derive instance Generic (TopLevel) _

instance Gql TopLevel where
  gql _ = object unit

newtype Book = Book
  { title :: String
  , price :: Number
  , author :: Unit -> GqlAff Author
  }

derive instance Generic (Book) _

instance Gql Book where
  gql _ = object unit


newtype Author = Author
  { name :: String
  , bio :: GqlAff String
  , books :: Unit -> Array (Book )
  }

derive instance Generic (Author) _

instance Gql Author where
  gql _ = object unit

io :: forall a. a -> GqlAff a
io = GqlIo <<< pure

resolveTestQuery :: Resolver Error GqlAff -> String -> Aff (Either GqlError (Result String))
resolveTestQuery resolver' query = evalGql mockRequest $ map (map message) <$> resolveQueryString resolver' query

mockRequest :: Request
mockRequest = unsafeCoerce unit