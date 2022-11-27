module Test.GraphQL.E2E (spec) where

import Prelude

import Data.Array (filter)
import Data.DateTime (DateTime(..), Time, canonicalDate)
import Data.Enum (class BoundedEnum)
import Data.Enum as Enum
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe, fromMaybe, maybe)
import Data.Newtype (class Newtype)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect, liftEffect)
import GraphQL.Resolver.GqlM (GqlM, gPure)
import GraphQL.Resolver.GqlObj (GqlObj(..))
import GraphQL.Server (GqlServerM, defaultOpts, liftServer, start)
import GraphQL.Server.Gql (class Gql, object)
import Test.GraphQL.E2E.Util (gqlReq, noErrors, shouldHaveData)
import Test.Spec (Spec, before, describe, it)

spec :: Spec Unit
spec =
  before
    (liftServer gqlServer) $ describe "Graphql server e2e tests" do
    it "should return top level fields" \endServer -> do
      res <- gqlReq "query t1 { top_level_pure_ints top_level_string }"
      noErrors res
      res `shouldHaveData`
        { top_level_pure_ints: [ 100 ]
        , top_level_string: "hello world"
        }
      done endServer
    it "should return user fields" \endServer -> do
      res <- gqlReq "query t1 { users { __typename id name } }"
      noErrors res
      res `shouldHaveData`
        { users:
            [ { __typename: "User"
              , id: 1
              , name: "Jane"
              }
            , { __typename: "User"
              , id: 2
              , name: "John"
              }
            , { __typename: "User"
              , id: 3
              , name: "Alex"
              }
            ]
        }
      done endServer
    it "should return user fields with arguments" \endServer -> do
      res <- gqlReq "query t1 { users(created_before: \"2020-01-01\") { __typename id name } }"
      noErrors res
      res `shouldHaveData`
        { users:
            [ { __typename: "User"
              , id: 1
              , name: "Jane"
              }
            ]
        }
      done endServer
    it "should return user fields with recursive fields" \endServer -> do
      res <- gqlReq
        """query t1 { 
        users(created_before: "2020-01-01") 
          { __typename 
            id 
            name 
          , orders { __typename id user { __typename id  } } 
          } 
      }"""
      noErrors res
      res `shouldHaveData`
        { users:
            [ { __typename: "User"
              , id: 1
              , name: "Jane"
              , orders:
                  [ { __typename: "Order"
                    , id: 1
                    , user:
                        { __typename: "User"
                        , id: 1
                        }
                    }
                  ]
              }
            ]
        }
      done endServer

done
  :: forall m
   . MonadEffect m
  => (Effect Unit -> Effect Unit)
  -> m Unit
done endServer = void $ liftEffect $ endServer (pure unit)

gqlServer ∷ GqlServerM Aff
gqlServer = start
  defaultOpts
  { query:
      { top_level_pure_ints: [ 100 ]
      , top_level_string: gPure "hello world"
      , users
      }
  , mutation: {}
  }

users
  :: { created_before :: Maybe DateTime
     , created_after :: Maybe DateTime
     }
  -> GqlM (Array User)
users args =
  pure
    $ filter (\(User u) -> maybe true (u.created_at > _) args.created_after)
    $ filter (\(User u) -> maybe true (u.created_at < _) args.created_before)
        [ user1
        , user2
        , user3
        ]

user1 :: User
user1 = User
  { id: 1
  , name: "Jane"
  , created_at: unsafeMakeDateTime 2019 1 1 bottom
  , orders: \{ id } -> pure $ filter (\(Order o) -> maybe true (eq o.id) id)
      [ Order
          { id: 1
          , user: user1
          , widget: Widget { id: 1, name: "widget1", ___a: "test", testObject: GqlObj { a: "" } }
          }
      ]
  }

user2 :: User
user2 = User
  { id: 2
  , name: "John"
  , created_at: unsafeMakeDateTime 2020 1 1 bottom
  , orders: \_ -> pure []
  }

user3 :: User
user3 = User
  { id: 3
  , name: "Alex"
  , created_at: unsafeMakeDateTime 2020 1 1 bottom
  , orders: \_ -> pure []
  }

newtype User = User
  { id :: Int
  , name :: String
  , created_at :: DateTime
  , orders :: { id :: Maybe Int } -> GqlM (Array Order)
  }

derive instance Generic User _

instance Gql User where
  gql _ = object unit

newtype Order = Order
  { id :: Int
  , user :: User
  , widget :: Widget
  }

derive instance Generic Order _

derive instance Newtype Order _

instance Gql Order where
  gql _ = object unit

newtype Widget = Widget
  { id :: Int
  , name :: String
  , ___a :: String
  , testObject :: TestObject
  }

derive instance Generic Widget _

derive instance Newtype Widget _

instance Gql Widget where
  gql _ = object unit

type TestObject = GqlObj "TestObject" { a :: String }

type T1 = GqlObj "T1" { a :: String, t2 :: T2 }

newtype T2 = T2 (GqlObj "T2" { a :: String, t1 :: T1 })

unsafeMakeDateTime :: Int -> Int -> Int -> Time -> DateTime
unsafeMakeDateTime year month day time = DateTime (canonicalDate (toEnum year) (toEnum month) (toEnum day)) time

toEnum :: forall e. BoundedEnum e => Int -> e
toEnum = fromMaybe bottom <<< Enum.toEnum
