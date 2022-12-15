module Test.GraphQL.Server.Branch (spec) where

import Prelude

import Control.Monad.Reader (ask, asks)
import Data.Argonaut (class EncodeJson, Json, encodeJson, jsonEmptyArray, jsonNull)
import Data.Either (either)
import Data.Filterable (filter)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff (Aff, error, throwError)
import Foreign.Object as Object
import GraphQL.Resolver (RootResolver, rootResolver)
import GraphQL.Server.Auth (Auth, auth)
import GraphQL.Server.Branch (class GqlIf, Branch(..))
import GraphQL.Server.Gql (class Gql, object)
import GraphQL.Server.GqlM (GqlM, runGqlM)
import GraphQL.Server.GqlResM as GqlM
import GraphQL.Server.HandleOperation (handleOperation)
import GraphQL.Server.HandleRequest (parseOperation)
import HTTPure (Request)
import Test.GraphQL.E2E.Util (JsonTest(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Unsafe.Coerce (unsafeCoerce)

spec :: Spec Unit
spec =
  describe "GraphQL.Server.Branch" do
    describe "Branch" do
      it "should resolve a simple query" do
        "query { branch_1 branch_2 { name } }" `shouldResolveTo`
          { branch_1: 1
          , branch_2: [ { name: "a" }, { name: "b" } ]
          }
      -- it "should fail to get an admin secret as a non admin" do
      --   "query { branch_1 branch_2 { name } }" `shouldResolveTo`
      --     { branch_1: 1
      --     , branch_2: [ { name: "a" }, { name: "b" } ]
      --     }
      it "should resolve a queryType introspection query as a non admin" do
        """
        query { 
          __schema { 
            queryType { 
              name
              fields {
                name
                type {
                  name
                  kind
                  ofType {
                    name
                    kind
                    ofType {
                      name
                      kind
                      ofType {
                        name
                        kind
                      }
                    }
                  }
                }
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
                      [ { "name": "branch_1"
                        , "type": encodeJson
                            { "name": jsonNull
                            , "kind": "NON_NULL"
                            , "ofType":
                                { "name": "Int"
                                , "kind": "SCALAR"
                                , "ofType": jsonNull
                                }
                            }
                        , "args": jsonEmptyArray
                        }
                      , { "name": "branch_2"
                        , "type": encodeJson
                            { "name": jsonNull
                            , "kind": "NON_NULL"
                            , "ofType":
                                { "name": jsonNull
                                , "kind": "LIST"
                                , "ofType":
                                    { "name": jsonNull
                                    , "kind": "NON_NULL"
                                    , "ofType":
                                        { "name": "Obj1"
                                        , "kind": "OBJECT"
                                        }
                                    }
                                }
                            }
                        , "args": jsonEmptyArray
                        }
                      ]
                  }
              }
          }

shouldResolveTo :: forall a. EncodeJson a => String -> a -> Aff Unit
shouldResolveTo query expected = do
  res <- resolveAsJson query
  JsonTest res `shouldEqual` JsonTest (encodeJson expected)

resolveAsJson :: String -> Aff Json
resolveAsJson = resolveAsJsonWithVars Object.empty

resolveAsJsonWithVars :: Object.Object Json -> String -> Aff Json
resolveAsJsonWithVars = resolveWithEnv
  { propTrue: true
  , propFalse: false
  , isAdmin: false
  }

resolveWithEnv :: Env -> Object.Object Json -> String -> Aff Json
resolveWithEnv env vars query = do
  op <- GqlM.toAff' $ parseOperation Nothing query
  eit <- runGqlM (\_ -> pure env) mockRequest vars $ handleOperation simpleResolver op
  res <- either (throwError <<< error <<< show) pure eit
  pure res.data

simpleResolver :: GqlM Env (RootResolver Env)
simpleResolver =
  rootResolver
    { query:
        { branch_1: Branch
            True
            objs
            1
        , branch_2: Branch
            False
            objs
            1
        , admin_secret: auth AdminOnly "admin secret"
        }

    , mutation: {}
    }

getObjs :: forall env. ObjArgs -> GqlM Env (Array Obj1)
getObjs args = pure $
  filter
    (\(Obj1 o) -> maybe true (eq o.id) args.id)
    objs

objs ∷ Array Obj1
objs =
  [ Obj1 { id: 1, name: "a" }
  , Obj1 { id: 2, name: "b" }
  ]

type ObjArgs = { id :: Maybe Int }

newtype Obj1 = Obj1
  { name :: String
  , id :: Int
  }

derive instance Generic Obj1 _

instance Gql env Obj1 where
  gql _ = object unit

newtype Obj2 = Obj2
  { name :: String
  , id :: Int
  , secret_string :: Auth AdminOnly String
  }

derive instance Generic Obj2 _

instance Gql Env Obj2 where
  gql _ = object unit

data AdminOnly = AdminOnly

instance GqlIf AdminOnly Env where
  gqlIf _ = asks _.env.isAdmin

type Env =
  { propTrue :: Boolean
  , propFalse :: Boolean
  , isAdmin :: Boolean
  }

data True = True

instance GqlIf True Env where
  gqlIf _ = do
    { env: { propTrue } } <- ask
    pure propTrue

data False = False

instance GqlIf False Env where
  gqlIf _ = do
    { env: { propFalse } } <- ask
    pure propFalse

mockRequest :: Request
mockRequest = unsafeCoerce unit