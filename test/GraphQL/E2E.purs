module Test.GraphQL.E2E where

import Prelude

import Effect (Effect)
import GraphQL.Server (defaultOpts, start)
import Test.Spec (Spec, describe, it)

spec :: Spec Unit
spec =
  describe "Graphql server e2e tests" do
    it "should return a list of users" do
      pure unit

gqlServer = start defaultOpts
  { query: {}
  , mutation: {}
  }
