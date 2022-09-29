module Test.GraphQL.Server.Schema.GetTypeDefinitions where

import Prelude

import Data.GraphQL.AST (FieldDefinition(..), FieldsDefinition(..), ListType(..), NamedType(..), NonNullType(..), Type(..))
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import GraphQL.Server.Schema.GetFieldsDefinition (getFieldsDefinition)
import GraphQL.Server.Schema.GetTypeDefinitions (getObjectTypeDefinitions)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

spec :: Spec Unit
spec =
  describe "GraphQL.Server.Schema.GetTypeDefinitions" do
    describe "getFieldsDefinition" do
      it "should get the fields of a simple record" do
        pure unit
        -- getObjectTypeDefinitions (Proxy :: Proxy { a :: Int, b :: Maybe String, c :: Maybe (Array Boolean) }) `shouldEqual`
        --   ( Nil
        --   )
     