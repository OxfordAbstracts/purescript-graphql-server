module Test.GraphQL.Server.Schema.Introspection (spec) where

import Prelude

import Data.Argonaut (class EncodeJson, encodeJson)
import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import GraphQL.Resolver.JsonResolver (resolveQueryString)
import GraphQL.Resolver.Result (Result(..))
import GraphQL.Server.GqlError (GqlError)
import GraphQL.Server.Schema.Introspection (makeIntrospectionResolver)
import GraphQL.Server.Schema.Introspection.Types (ISchema(..), IType(..), ITypeKind(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec =
  describe "GraphQL.Server.Schema.Introspection" do
    describe "makeIntrospectionResolver" do
      it "should make the schema resolver from an instrospection schema record" do
        res <- resolveSchema simpleSchema "{ __schema { queryType { name kind ofType } } }"
        res `shouldEqual`
          ( Right
              ( ResultObject
                  ( ( Tuple "__schema"
                        ( ResultObject
                            ( ( Tuple "queryType"
                                  ( ResultObject
                                      ( ( Tuple "name"
                                            (ResultNullable (Just (leaf "my_query_type")))
                                        )
                                          :
                                            ( Tuple "kind" (leaf "SCALAR")
                                            )
                                          :
                                            ( Tuple "ofType" (ResultNullable Nothing)
                                            )
                                          : Nil
                                      )
                                  )
                              ) : Nil
                            )
                        )
                    ) : Nil
                  )
              )
          )
      it "should make the types resolver from an instrospection schema record" do
        res <- resolveSchema simpleSchema "{ __type(name: \"my_query_type\") { name } }"
        res `shouldEqual`
          ( Right
              ( ResultObject
                  ( ( Tuple "__type"
                        ( ResultNullable $ Just
                            ( ResultObject
                                ( ( Tuple "name"
                                      (ResultNullable (Just (leaf "my_query_type")))
                                  )
                                    : Nil
                                )
                            )
                        )
                    ) : Nil
                  )
              )
          )

leaf ∷ ∀ a. EncodeJson a ⇒ a → Result
leaf = ResultLeaf <<< encodeJson

resolveSchema :: ISchema -> String -> Aff (Either GqlError Result)
resolveSchema resolver query = resolveQueryString (makeIntrospectionResolver resolver) query

simpleSchema :: ISchema
simpleSchema = ISchema $
  { types: queryType : Nil
  , queryType
  , mutationType: Nothing
  , subscriptionType: Nothing
  , directives: Nil
  }
  where
  queryType = IType
    { description: Nothing
    , enumValues: \_ -> Nothing
    , fields: \_ -> Nothing
    , inputFields: Nothing
    , interfaces: Nothing
    , kind: SCALAR
    , name: Just "my_query_type"
    , ofType: Nothing
    , possibleTypes: Nothing
    }