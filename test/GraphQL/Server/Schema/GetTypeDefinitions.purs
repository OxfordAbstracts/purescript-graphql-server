module Test.GraphQL.Server.Schema.GetTypeDefinitions where

import Prelude

import Data.GraphQL.AST (FieldDefinition(..), FieldsDefinition(..), NamedType(..), NonNullType(..), ObjectTypeDefinition(..), Type(..))
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import GraphQL.Resolver.Resolver.GqlObject (GqlObj)
import GraphQL.Server.Schema.GetTypeDefinitions (getObjectTypeDefinitions)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

spec :: Spec Unit
spec =
  describe "GraphQL.Server.Schema.GetTypeDefinitions" do
    describe "getObjectTypeDefinitions" do
      it "should get an empty object type definition from a single empty `GqlObj`" do
        getObjectTypeDefinitions (Proxy :: Proxy (GqlObj "name" {})) `shouldEqual`
          ( ObjectTypeDefinition
              { description: Nothing
              , directives: Nothing
              , fieldsDefinition: Just (FieldsDefinition Nil)
              , implementsInterfaces: Nothing
              , name: "name"
              }
              : Nil
          )

      it "should get a type definition from a single singleton `GqlObj`" do
        getObjectTypeDefinitions (Proxy :: Proxy (GqlObj "name" { a :: Int })) `shouldEqual`
          ( ObjectTypeDefinition
              { description: Nothing
              , directives: Nothing
              , fieldsDefinition: Just
                  ( FieldsDefinition $
                      FieldDefinition
                        { argumentsDefinition: Nothing
                        , description: Nothing
                        , directives: Nothing
                        , name: "a"
                        , type: Type_NonNullType $ NonNullType_NamedType $ NamedType "Int"
                        } : Nil
                  )
              , implementsInterfaces: Nothing
              , name: "name"
              }
              : Nil
          )
      it "should get a type definition from a nested  `GqlObj`" do
        getObjectTypeDefinitions
          ( Proxy
              :: Proxy
                   ( GqlObj "name"
                       { a :: Int
                       , child_prop :: GqlObj "child_name" { b :: Maybe String }
                       }
                   )
          ) `shouldEqual`
          ( ObjectTypeDefinition
              { description: Nothing
              , directives: Nothing
              , fieldsDefinition: Just
                  ( FieldsDefinition $
                      FieldDefinition
                        { argumentsDefinition: Nothing
                        , description: Nothing
                        , directives: Nothing
                        , name: "a"
                        , type: Type_NonNullType $ NonNullType_NamedType $ NamedType "Int"
                        }
                        : FieldDefinition
                            { argumentsDefinition: Nothing
                            , description: Nothing
                            , directives: Nothing
                            , name: "child_prop"
                            , type: Type_NonNullType $ NonNullType_NamedType $ NamedType "child_name"
                            }
                        : Nil
                  )
              , implementsInterfaces: Nothing
              , name: "name"
              }
              :
                ObjectTypeDefinition
                  { description: Nothing
                  , directives: Nothing
                  , fieldsDefinition: Just
                      ( FieldsDefinition $
                          FieldDefinition
                            { argumentsDefinition: Nothing
                            , description: Nothing
                            , directives: Nothing
                            , name: "b"
                            , type: Type_NamedType $ NamedType "String"
                            }
                            : Nil
                      )
                  , implementsInterfaces: Nothing
                  , name: "child_name"
                  }
              : Nil
          )