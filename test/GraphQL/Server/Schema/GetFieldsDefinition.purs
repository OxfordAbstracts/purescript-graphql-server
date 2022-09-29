module Test.GraphQL.Server.Schema.GetFieldsDefinition where

import Prelude

import Data.GraphQL.AST (ArgumentsDefinition(..), FieldDefinition(..), FieldsDefinition(..), InputValueDefinition(..), ListType(..), NamedType(..), NonNullType(..), Type(..))
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import GraphQL.Server.Schema.GetFieldsDefinition (getFieldsDefinitions)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

spec :: Spec Unit
spec =
  describe "GraphQL.Server.Schema.GetFieldsDefinition" do
    describe "getFieldsDefinitions" do
      it "should get the fields of a simple record" do
        getFieldsDefinitions (Proxy :: Proxy { a :: Int, b :: Maybe String, c :: Maybe (Array Boolean) }) `shouldEqual`
          ( FieldsDefinition $
              FieldDefinition
                { argumentsDefinition: Nothing
                , description: Nothing
                , directives: Nothing
                , name: "a"
                , type: Type_NonNullType $ NonNullType_NamedType $ NamedType "Int"
                }
                :
                  FieldDefinition
                    { argumentsDefinition: Nothing
                    , description: Nothing
                    , directives: Nothing
                    , name: "b"
                    , type: Type_NamedType $ NamedType "String"
                    }
                :
                  FieldDefinition
                    { argumentsDefinition: Nothing
                    , description: Nothing
                    , directives: Nothing
                    , name: "c"
                    , type:
                        Type_ListType
                          $ ListType
                          $ Type_NonNullType
                          $ NonNullType_NamedType
                          $ NamedType "Boolean"
                    }

                :
                  Nil
          )
      it "should get the fields with arguments" do
        getFieldsDefinitions
          ( Proxy
              :: Proxy
                   { a :: { arg1 :: Int, arg2 :: Maybe String } -> Int
                   }
          ) `shouldEqual`
          ( FieldsDefinition $
              FieldDefinition
                { argumentsDefinition: Just $ ArgumentsDefinition $
                    InputValueDefinition
                      { defaultValue: Nothing
                      , description: Nothing
                      , directives: Nothing
                      , name: "arg1"
                      , type: Type_NonNullType $ NonNullType_NamedType $ NamedType "Int"
                      }
                      :
                        InputValueDefinition
                          { defaultValue: Nothing
                          , description: Nothing
                          , directives: Nothing
                          , name: "arg2"
                          , type: Type_NamedType $ NamedType "String"
                          }
                      :
                        Nil
                , description: Nothing
                , directives: Nothing
                , name: "a"
                , type: Type_NonNullType $ NonNullType_NamedType $ NamedType "Int"
                }
                :
                  Nil
          )