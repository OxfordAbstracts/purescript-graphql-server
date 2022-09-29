module Test.GraphQL.Server.Resolver.ToResolver where

import Prelude

import Data.Argonaut (class EncodeJson, encodeJson)
import Data.Array (filter)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Int (toNumber)
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe, maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.String (toUpper)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, Fiber, joinFiber)
import GraphQL.Resolver.GqlIo (GqlFiber, GqlIo)
import GraphQL.Resolver.JsonResolver (resolveQueryString)
import GraphQL.Resolver.Resolver.GqlObject (GqlObj(..))
import GraphQL.Resolver.Result (Result(..))
import GraphQL.Resolver.ToResolver (class ToResolver, toResolver)
import GraphQL.Server.GqlError (GqlError)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec =
  describe "GraphQL.Server.Resolver.ToResolver" do
    describe "toResolver" do
      it "should create simple immediate resolvers" do
        let resolver = (GqlObj { a: 1, b: 2.0 })
        resA <- resolveTyped resolver "{a}"
        resA `shouldEqual` Right (ResultObject $ pure $ Tuple "a" $ leaf 1)
        resB <- resolveTyped resolver "{b}"
        resB `shouldEqual` Right (ResultObject $ pure $ Tuple "b" $ leaf 2.0)
        resAll <- resolveTyped resolver "{a b}"
        resAll `shouldEqual` Right (ResultObject ((Tuple "a" (leaf 1)) : (Tuple "b" (leaf 2)) : Nil))
      it "should create simple async resolvers" do
        let resolver = (GqlObj { a: aff 1, b: aff 2.0 })
        resA <- resolveTyped resolver "{a}"
        resA `shouldEqual` Right (ResultObject $ pure $ Tuple "a" $ leaf 1)
        resB <- resolveTyped resolver "{b}"
        resB `shouldEqual` Right (ResultObject $ pure $ Tuple "b" $ leaf 2.0)
        resAll <- resolveTyped resolver "{a b}"
        resAll `shouldEqual` Right (ResultObject ((Tuple "a" (leaf 1)) : (Tuple "b" (leaf 2)) : Nil))
      it "should create resolvers with arguments" do
        let

          expectedA = Tuple "double" $ leaf 6
          expectedB = Tuple "shout" $ leaf "HELLO"
          expectedC = Tuple "async" $ leaf "HELLO"
          resolve = resolveTyped resolverParent

        resA <- resolve "{double(a: 3)}"
        resA `shouldEqual` Right (ResultObject $ pure expectedA)
        resB <- resolve "{shout(str: \"hello\")}"
        resB `shouldEqual` Right (ResultObject $ pure expectedB)
        resC <- resolve "{async(str: \"hello\")}"
        resC `shouldEqual` Right (ResultObject $ pure expectedC)
        resAll <- resolve "{double(a: 3) shout(str: \"hello\") async(str: \"hello\")}"
        resAll `shouldEqual` Right (ResultObject $ List.fromFoldable [ expectedA, expectedB, expectedC ])

        resNoArgs <- resolve "{noArgs}"
        resNoArgs `shouldEqual` Right (ResultObject $ pure $ Tuple "noArgs" $ leaf "no args")
      it "should create resolvers that return arrays" do
        res <- resolveTyped resolverParent "{ints(min: 3)}"
        res `shouldEqual` Right (ResultObject $ pure $ Tuple "ints" $ leaf [ 3, 4, 5 ])
      it "should create nested resolvers" do
        res <- resolveTyped resolverParent "{child1 {id}}"
        res `shouldEqual` Right (ResultObject $ pure $ Tuple "child1" $ ResultObject $ pure $ Tuple "id" $ leaf 1)
      it "should create nested array resolvers" do
        res <- resolveTyped resolverParent "{children1(ids: [1,2,3]) {id, n}}"
        res `shouldEqual` Right
          ( ResultObject $ pure $ Tuple "children1" $ ResultList $ List.fromFoldable
              [ ResultObject $ List.fromFoldable
                  [ Tuple "id" $ leaf 1
                  , Tuple "n" $ leaf 1.0
                  ]
              , ResultObject $ List.fromFoldable
                  [ Tuple "id" $ leaf 2
                  , Tuple "n" $ leaf 2.0
                  ]
              , ResultObject $ List.fromFoldable
                  [ Tuple "id" $ leaf 3
                  , Tuple "n" $ leaf 3.0
                  ]
              ]
          )
      it "should create recursive resolvers" do
        res <- resolveTyped recursiveChild1 "{id children {id, child {id}}}"
        res `shouldEqual`
          ( ( Right
                ( ResultObject
                    ( (Tuple "id" (leaf 1))
                        :
                          ( Tuple "children"
                              ( ResultList
                                  ( ( ResultObject
                                        ( (Tuple "id" (leaf 2))
                                            : (Tuple "child" (ResultObject ((Tuple "id" (leaf 1)) : Nil)))
                                            : Nil
                                        )
                                    )
                                      : Nil
                                  )
                              )
                          )
                        : Nil
                    )
                )
            )
          )

resolverParent
  :: GqlObj "ResolverParent"
       { async :: { str :: String } -> GqlFiber String
       , double :: { a :: Int } -> Int
       , noArgs :: GqlFiber String
       , shout :: { str :: String } -> String
       , ints :: { min :: Maybe Int, max :: Maybe Int } -> GqlFiber (Array Int)
       , child1 :: ResolverChild1
       , children1 :: { ids :: Array Int } -> (Array ResolverChild1)
       }
resolverParent =
  ( GqlObj
      { double: \({ a }) -> a * 2
      , shout: \({ str }) -> toUpper str
      , async: \({ str }) -> pure $ toUpper str
      , noArgs: pure "no args"
      , ints: \{ min, max } -> pure $
          [ 1, 2, 3, 4, 5 ]
            # filter \i -> maybe true (i >= _) min && maybe true (i <= _) max
      , child1: resolverChild1
      , children1: \{ ids } -> map mkChild ids
      }
  )

type ResolverChild1 = GqlObj "ResolverChild1"
  { id :: Int
  , n :: GqlFiber Number
  , name :: String
  }

resolverChild1 :: ResolverChild1
resolverChild1 = mkChild 1

mkChild :: Int -> ResolverChild1
mkChild = \id ->
  GqlObj
    { id
    , n: pure $ toNumber id
    , name: "child " <> show id
    }

newtype RecursiveChild1 = RecursiveChild1
  { id :: Int
  , n :: GqlFiber Number
  , name :: String
  , children :: Unit -> Array RecursiveChild2
  }

derive instance Newtype RecursiveChild1 _

instance ToResolver RecursiveChild1 (GqlIo Fiber) where
  toResolver a = toResolver (unwrap a)

recursiveChild1 :: RecursiveChild1
recursiveChild1 =
  RecursiveChild1
    { id: 1
    , n: pure $ toNumber 1
    , name: "child 1"
    , children: \_ -> [ recursiveChild2 ]
    }

newtype RecursiveChild2 = RecursiveChild2
  { id :: Int
  , n :: GqlFiber Number
  , name :: String
  , child :: Unit -> RecursiveChild1
  }

derive instance Generic RecursiveChild2 _

derive instance Newtype RecursiveChild2 _
instance ToResolver RecursiveChild2 (GqlIo Fiber) where
  toResolver a = toResolver (unwrap a)

recursiveChild2 :: RecursiveChild2
recursiveChild2 = RecursiveChild2
  { id: 2
  , n: pure $ toNumber 2
  , name: "child 2"
  , child: \_ -> recursiveChild1
  }

leaf ∷ ∀ (a ∷ Type). EncodeJson a ⇒ a → Result
leaf = ResultLeaf <<< encodeJson

aff :: forall a. a -> GqlFiber a
aff = pure

resolveTypedAff :: forall a. ToResolver a GqlFiber => a -> String -> GqlFiber (Either GqlError Result)
resolveTypedAff resolver query = resolveQueryString (toResolver resolver) query

resolveTyped :: forall a. ToResolver a GqlFiber => a -> String -> Aff (Either GqlError Result)
resolveTyped resolver query = joinFiber $ unwrap $ resolveTypedAff resolver query