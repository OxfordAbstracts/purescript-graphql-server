module Test.GraphQL.Server.Resolver.ToResolver where

import Prelude

import Data.Argonaut (class EncodeJson, encodeJson, jsonNull)
import Data.Either (Either(..))
import Data.Foldable (class Foldable)
import Data.List (List(..), (:))
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import GraphQL.Newtypes.ToResolver (class ToJsonResolver, toJsonResolver)
import GraphQL.Resolver.Newtypes.ResolveTo (GqlObj(..))
import GraphQL.Resolver.Untyped (Field, Resolver(..), Result(..), resolveQueryString)
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
        let resolver = (GqlObj { double: \({ a } :: { a :: Int }) -> a * 2 })
        resA <- resolveTyped resolver "{double(a: 3)}"
        resA `shouldEqual` Right (ResultObject $ pure $ Tuple "double" $ leaf 6)

leaf ∷ ∀ (a ∷ Type). EncodeJson a ⇒ a → Result
leaf = ResultLeaf <<< encodeJson

aff :: forall a. a -> Aff a
aff = pure

resolveTyped :: forall a. ToJsonResolver a Aff => a -> String -> Aff (Either GqlError Result)
resolveTyped resolver query = resolveQueryString (toJsonResolver resolver) query