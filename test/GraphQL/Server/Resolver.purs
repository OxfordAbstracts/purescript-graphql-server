module Test.GraphQL.Server.Resolver (spec) where

import Prelude

import Data.Argonaut (class EncodeJson, encodeJson, stringify)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import GraphQL.Server.GqlResM (GqlResM, toAff)
import GraphQL.Server.Resolver (class Resolver, resolve)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec =
  describe "GraphQL.Server.Resolver" do
    describe "resolve" do
      describe "Node resolvers" do
        describe "happy path" do
          it "Int" do
            1 `shouldResolveNodeTo` 1

          it "String" do
            "test" `shouldResolveNodeTo` "test"

          it "(Array Number)" do
            [ 1.0, 2.0 ] `shouldResolveNodeTo` [ 1.0, 2.0 ]

-- it "single field record" do
--   { foo: "bar" } `shouldResolveNodeTo` { foo: "bar" }

shouldResolveNodeTo :: forall resolver result. EncodeJson resolver => Resolver resolver => EncodeJson result => resolver -> result -> Aff Unit
shouldResolveNodeTo input output =
  resolve (gqlM input) Nothing `shouldStringifyAs` (pure $ encodeJson output)

shouldStringifyAs :: forall a. EncodeJson a => GqlResM a -> GqlResM a -> Aff Unit
shouldStringifyAs l r = do
  l' <- toAff l
  r' <- toAff r
  toJsonStr l' `shouldEqual` toJsonStr r'

  where
  toJsonStr = map (stringify <<< encodeJson)

-- simpleResolver :: 

gqlM :: forall a. a -> GqlResM a
gqlM = pure
