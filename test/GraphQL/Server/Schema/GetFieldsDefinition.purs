module Test.GraphQL.Server.Schema.GetFieldsDefinition where

import Prelude

import Data.GraphQL.AST (FieldDefinition(..), FieldsDefinition(..), NamedType(..), NonNullType(..), Type(..))
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import GraphQL.Server.Schema.GetFieldsDefinitions (getFieldsDefinitions)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

spec :: Spec Unit
spec =
  describe "GraphQL.Server.Schema.GetFieldsDefinition" do
    describe "getFieldsDefinitions" do
      it "should get the fields of a simple object" do
        getFieldsDefinitions (Proxy :: Proxy { a :: Int, b :: Maybe String }) `shouldEqual`
          ( FieldsDefinition $
              FieldDefinition
                { argumentsDefinition: Nothing --  :: Maybe ArgumentsDefinition
                , description: Nothing --  :: Maybe String
                , directives: Nothing --  :: Maybe Directives
                , name: "a"
                , type: Type_NonNullType $ NonNullType_NamedType $ NamedType "Int"
                }
                :
                  FieldDefinition
                    { argumentsDefinition: Nothing --  :: Maybe ArgumentsDefinition
                    , description: Nothing --  :: Maybe String
                    , directives: Nothing --  :: Maybe Directives
                    , name: "b"
                    , type: Type_NamedType $ NamedType "String"
                    }

                :
                  Nil
          )