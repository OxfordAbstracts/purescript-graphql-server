module Test.GraphQL.Server.Resolver.ToResolver where

import Prelude

import Data.Argonaut (class EncodeJson, encodeJson)
import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.List as List
import Data.String (toUpper)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import GraphQL.Newtypes.ToResolver (class ToJsonResolver, toJsonResolver)
import GraphQL.Resolver.Newtypes.ResolveTo (GqlObj(..))
import GraphQL.Resolver.Untyped (Result(..), resolveQueryString)
import GraphQL.Server.GqlError (GqlError)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec =
  describe "GraphQL.Server.Resolver.ToResolver" do
    describe "toJsonResolver" do
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
          resolver =
            ( GqlObj
                { double: \({ a } :: { a :: Int }) -> a * 2
                , shout: \({ str } :: { str :: String }) -> toUpper str
                , async: \({ str } :: { str :: String }) -> aff $ toUpper str
                , noArgs: aff "no args"
                }
            )

          expectedA = Tuple "double" $ leaf 6
          expectedB = Tuple "shout" $ leaf "HELLO"
          expectedC = Tuple "async" $ leaf "HELLO"

        resA <- resolveTyped resolver "{double(a: 3)}"
        resA `shouldEqual` Right (ResultObject $ pure expectedA)
        resB <- resolveTyped resolver "{shout(str: \"hello\")}"
        resB `shouldEqual` Right (ResultObject $ pure expectedB)
        resC <- resolveTyped resolver "{async(str: \"hello\")}"
        resC `shouldEqual` Right (ResultObject $ pure expectedC)
        resAll <- resolveTyped resolver "{double(a: 3) shout(str: \"hello\") async(str: \"hello\")}"
        resAll `shouldEqual` Right (ResultObject $ List.fromFoldable [expectedA, expectedB, expectedC])

        resNoArgs <- resolveTyped resolver "{noArgs}"
        resNoArgs `shouldEqual` Right (ResultObject $ pure $ Tuple "noArgs" $ leaf "no args")


leaf ∷ ∀ (a ∷ Type). EncodeJson a ⇒ a → Result
leaf = ResultLeaf <<< encodeJson

aff :: forall a. a -> Aff a
aff = pure

resolveTyped :: forall a. ToJsonResolver a Aff => a -> String -> Aff (Either GqlError Result)
resolveTyped resolver query = resolveQueryString (toJsonResolver resolver) query