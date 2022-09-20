module Test.GraphQL.Server.Resolver.Untyped where

import Prelude

import Data.Argonaut (class EncodeJson, encodeJson, jsonNull)
import Data.Either (Either(..))
import Data.Foldable (class Foldable)
import Data.List (List(..), (:))
import Data.Map as Map
import Data.Tuple (Tuple(..))
import GraphQL.Resolver.Untyped (Resolver(..), Result(..), Field, resolveQueryString)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec =
  describe "GraphQL.Server.Resolver.Untyped" do
    describe "resolveQueryString" do
      it "should resolve a flat query string" do
        let

          query = "{ top_level_1 top_level_2 top_level_3 }"

          expected :: Result
          expected = ResultObject
            $ Tuple "top_level_1" (ResultLeaf $ encodeJson "top_val_1")
                : Tuple "top_level_2" (ResultLeaf $ encodeJson [ 1 ])
                : Tuple "top_level_3" (ResultLeaf $ encodeJson { "l2": 2, "l1": "v1" })
                : Nil

        actual <- resolveQueryString resolver query
        actual `shouldEqual` Right expected
      it "should resolve a nested query string" do
        let
          query = "{ top_level_nested_1 {c1 c2} }"

          expected :: Result
          expected = ResultObject
            $ pure
            $ Tuple "top_level_nested_1"
            $ ResultObject
            $ Tuple "c1" (ResultLeaf $ encodeJson "v11")
                : Tuple "c2" (ResultLeaf $ encodeJson "v12")
                : Nil

        actual <- resolveQueryString resolver query
        actual `shouldEqual` Right expected

resolver :: forall m. Applicative m => Resolver m
resolver = Fields
  { fields:
      mkFieldMap
        [ { args: mempty
          , name: "top_level_1"
          , resolver: resolveNode "top_val_1"
          }
        , { args: mempty
          , name: "top_level_2"
          , resolver: resolveNode [ 1 ]
          }
        , { args: mempty
          , name: "top_level_3"
          , resolver: resolveNode { l1: "v1", l2: 2 }
          }
        , { args: mempty
          , name: "top_level_nested_1"
          , resolver: \_ -> Fields
              { fields: mkFieldMap
                  [ { args: mempty
                    , name: "c1"
                    , resolver: resolveNode "v11"
                    }
                  , { args: mempty
                    , name: "c2"
                    , resolver: resolveNode "v12"
                    }
                  ]
              }
          }
        ]
  }

resolveNode ∷ ∀ (args ∷ Type) (m ∷ Type -> Type) (a ∷ Type). Applicative m ⇒ EncodeJson a ⇒ a → args → Resolver m
resolveNode a _ = Node $ pure $ encodeJson a

mkFieldMap
  :: forall f m
   . Foldable f
  => Functor f
  => f (Field m)
  -> Map.Map String (Field m)
mkFieldMap = Map.fromFoldable <<< map (\f -> Tuple f.name f)