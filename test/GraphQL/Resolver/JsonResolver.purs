module Test.GraphQL.Server.Resolver.JsonResolver (spec) where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Reader (class MonadAsk, ReaderT, runReaderT)
import Control.Parallel (class Parallel, parallel, sequential)
import Data.Argonaut (class EncodeJson, encodeJson)
import Data.Either (Either(..))
import Data.Filterable (filter)
import Data.Foldable (class Foldable)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.Map as Map
import Data.Maybe (Maybe, maybe)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, ParAff)
import Effect.Exception (Error, message)
import GraphQL.Resolver.EvalGql (class EvalGql, evalGql)
import GraphQL.Resolver.GqlIo (GqlIo(..))
import GraphQL.Resolver.JsonResolver (Field, Resolver(..), resolveQueryString)
import GraphQL.Resolver.Result (Result(..))
import GraphQL.Resolver.ToResolver (class ToResolver, toObjectResolver, toResolver)
import GraphQL.Server.GqlError (GqlError, FailedToResolve(..))
import GraphQL.Server.GqlRep (class GqlRep, GObject)
import HTTPure (Request)
import HTTPure as Headers
import HTTPure.Headers (Headers)
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
        res <- evalGql mockRequest $ resolveTestQuery booksResolver
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

booksResolver :: forall err. Resolver err GqlCustom
booksResolver =
  toResolver $ gqlObj
    { books
    }
  where
  books = \(opts :: { maxPrice :: Maybe Number }) -> do
    io $
      filter (\(Book b) -> maybe true (b.price <= _) opts.maxPrice)
        books_

  author = Author
    { name: "Iain M. Banks"
    , bio: io "This is some stuff about the author"
    , books: \_ -> books_
    }

  books_ =
    [ Book
        { title: "State of the Art"
        , price: 9.99
        , author: \_ -> io author
        }
    , Book
        { title: "Consider Phlebas"
        , price: 5.99
        , author: \_ -> io author
        }
    ]

newtype Book m = Book
  { title :: String
  , price :: Number
  , author :: Unit -> m (Author m)
  }

derive instance Generic (Book m) _

instance GqlRep (Book a) GObject "Book"

instance Applicative m => ToResolver err (Book (GqlIo m)) (GqlIo m) where
  toResolver a = toObjectResolver a

newtype Author m = Author
  { name :: String
  , bio :: m String
  , books :: Unit -> Array (Book m)
  }

derive instance Generic (Author m) _

instance GqlRep (Author a) GObject "Author"

instance Applicative m => ToResolver err (Author (GqlIo m)) (GqlIo m) where
  toResolver a = toObjectResolver a

io :: forall a. a -> GqlCustom a
io = GqlIo <<< pure

resolveTestQuery :: Resolver Error GqlCustom -> String -> Aff (Either GqlError (Result String))
resolveTestQuery resolver' query = evalGql mockRequest $ map (map message) <$> resolveQueryString resolver' query

mockRequest :: Request
mockRequest = unsafeCoerce
  { headers: Headers.headers
      [ Tuple "key1" "val1"
      , Tuple "key2" "val2"
      ]
  }

type GqlCustom = GqlIo CustomM

newtype CustomM a = CustomM (ReaderT Ctx Aff a)

newtype CustomParM a = CustomParM (ReaderT Ctx ParAff a)

type Ctx = { headers :: Headers }

derive instance Newtype (CustomM a) _
derive instance Functor CustomM
derive newtype instance Apply CustomM
derive newtype instance Applicative CustomM
derive newtype instance Bind CustomM
derive newtype instance Monad CustomM
derive newtype instance MonadThrow Error CustomM
derive newtype instance MonadError Error CustomM
derive newtype instance MonadAsk Ctx CustomM

derive instance Newtype (CustomParM a) _
derive instance Functor CustomParM
derive newtype instance Apply CustomParM
derive newtype instance Applicative CustomParM

instance Parallel CustomParM CustomM where
  parallel (CustomM a) = CustomParM $ parallel a
  sequential (CustomParM a) = CustomM $ sequential a

instance EvalGql CustomM where
  evalGql { headers } (CustomM a) = runReaderT a { headers }