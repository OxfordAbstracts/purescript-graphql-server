module Test.GraphQL.Server.Resolver.ToResolver where

import Prelude

import Data.Argonaut (class EncodeJson, encodeJson)
import Data.Array (filter)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Int (toNumber)
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe, maybe)
import Data.String (toUpper)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, Error, message)
import GraphQL.Resolver.GqlIo (GqlAff)
import GraphQL.Resolver.Gqlable (toAff)
import GraphQL.Resolver.JsonResolver (resolveQueryString)
import GraphQL.Resolver.Result (Result(..))
import GraphQL.Resolver.ToResolver (class ToResolver, FieldMap, ToResolverProps, toObjectResolver, toResolver)
import GraphQL.Server.GqlError (GqlError)
import GraphQL.Server.GqlRep (class GqlRep, GObject)
import GraphQL.Server.Schema.Introspection.GetType (class GetIFields, class GetGqlType, getObjectType)
import HTTPure (Request)
import Heterogeneous.Folding (class HFoldlWithIndex)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Unsafe.Coerce (unsafeCoerce)

spec :: Spec Unit
spec =
  describe "GraphQL.Server.Resolver.ToResolver" do
    describe "toResolver" do
      it "should create simple immediate resolvers" do
        let resolver = (gqlObj { a: 1, b: 2.0 })
        resA <- resolveTyped resolver "{a}"
        resA `shouldEqual` Right (ResultObject $ pure $ Tuple "a" $ leaf 1)
        resB <- resolveTyped resolver "{b}"
        resB `shouldEqual` Right (ResultObject $ pure $ Tuple "b" $ leaf 2.0)
        resAll <- resolveTyped resolver "{a b}"
        resAll `shouldEqual` Right (ResultObject ((Tuple "a" (leaf 1)) : (Tuple "b" (leaf 2)) : Nil))
      it "should create simple async resolvers" do
        let resolver = (gqlObj { a: aff 1, b: aff 2.0 })
        resA <- resolveTyped resolver "{a}"
        resA `shouldEqual` Right (ResultObject $ pure $ Tuple "a" $ leaf 1)
        resB <- resolveTyped resolver "{b}"
        resB `shouldEqual` Right (ResultObject $ pure $ Tuple "b" $ leaf 2.0)
        resAll <- resolveTyped resolver "{a b}"
        resAll `shouldEqual` Right (ResultObject ((Tuple "a" (leaf 1)) : (Tuple "b" (leaf 2)) : Nil))
      it "should create resolvers with arguments" do
        let
          expectedA = Tuple "double" $ leaf 6
          expectedB = Tuple "shout" $ leaf "HELLO"
          expectedC = Tuple "async" $ leaf "HELLO"
          resolve = resolveTyped resolverParent

        resA <- resolve "{double(a: 3)}"
        resA `shouldEqual` Right (ResultObject $ pure expectedA)
        resB <- resolve "{shout(str: \"hello\")}"
        resB `shouldEqual` Right (ResultObject $ pure expectedB)
        resC <- resolve "{async(str: \"hello\")}"
        resC `shouldEqual` Right (ResultObject $ pure expectedC)
        resAll <- resolve "{double(a: 3) shout(str: \"hello\") async(str: \"hello\")}"
        resAll `shouldEqual` Right (ResultObject $ List.fromFoldable [ expectedA, expectedB, expectedC ])

        resNoArgs <- resolve "{noArgs}"
        resNoArgs `shouldEqual` Right (ResultObject $ pure $ Tuple "noArgs" $ leaf "no args")
      it "should create resolvers that return arrays" do
        res <- resolveTyped resolverParent "{ints(min: 3)}"
        res `shouldEqual` Right (ResultObject $ pure $ Tuple "ints" $ ResultList (leaf 3 : leaf 4 : leaf 5 : Nil))
      it "should create nested resolvers" do
        res <- resolveTyped resolverParent "{child1 {id}}"
        res `shouldEqual` Right (ResultObject $ pure $ Tuple "child1" $ ResultObject $ pure $ Tuple "id" $ leaf 1)
      it "should create nested array resolvers" do
        res <- resolveTyped resolverParent "{children1(ids: [1,2,3]) {id, n}}"
        res `shouldEqual` Right
          ( ResultObject $ pure $ Tuple "children1" $ ResultList $ List.fromFoldable
              [ ResultObject $ List.fromFoldable
                  [ Tuple "id" $ leaf 1
                  , Tuple "n" $ leaf 1.0
                  ]
              , ResultObject $ List.fromFoldable
                  [ Tuple "id" $ leaf 2
                  , Tuple "n" $ leaf 2.0
                  ]
              , ResultObject $ List.fromFoldable
                  [ Tuple "id" $ leaf 3
                  , Tuple "n" $ leaf 3.0
                  ]
              ]
          )

gqlObj :: forall a115. a115 -> TestGqlObj a115
gqlObj = TestGqlObj

newtype TestGqlObj a = TestGqlObj a

derive instance Generic (TestGqlObj a) _

instance GqlRep (TestGqlObj a) GObject "TestGqlObj"

instance (Applicative m, HFoldlWithIndex (ToResolverProps err m) (FieldMap err m) { | a } (FieldMap err m)) => ToResolver err (TestGqlObj { | a }) m where
  toResolver a = toObjectResolver a

instance GetIFields { | a } => GetGqlType (TestGqlObj { | a }) where
  getType a = getObjectType a

resolverParent
  :: TestGqlObj
       { async :: { str :: String } -> GqlAff String
       , double :: { a :: Int } -> Int
       , noArgs :: GqlAff String
       , shout :: { str :: String } -> String
       , ints :: { min :: Maybe Int, max :: Maybe Int } -> GqlAff (Array Int)
       , child1 :: ResolverChild1
       , children1 :: { ids :: Array Int } -> (Array ResolverChild1)
       }
resolverParent =
  ( TestGqlObj
      { double: \({ a }) -> a * 2
      , shout: \({ str }) -> toUpper str
      , async: \({ str }) -> pure $ toUpper str
      , noArgs: pure "no args"
      , ints: \{ min, max } -> pure $
          [ 1, 2, 3, 4, 5 ]
            # filter \i -> maybe true (i >= _) min && maybe true (i <= _) max
      , child1: resolverChild1
      , children1: \{ ids } -> map mkChild ids
      }
  )

type ResolverChild1 = TestGqlObj
  { id :: Int
  , n :: GqlAff Number
  , name :: String
  }

resolverChild1 :: ResolverChild1
resolverChild1 = mkChild 1

mkChild :: Int -> ResolverChild1
mkChild = \id ->
  TestGqlObj
    { id
    , n: pure $ toNumber id
    , name: "child " <> show id
    }

leaf ∷ ∀ (a ∷ Type) err. EncodeJson a ⇒ a → (Result err)
leaf = ResultLeaf <<< encodeJson

aff :: forall a. a -> GqlAff a
aff = pure

resolveTypedFiber :: forall a. ToResolver Error a GqlAff => a -> String -> GqlAff (Either GqlError (Result Error))
resolveTypedFiber resolver query = resolveQueryString (toResolver resolver) query

resolveTyped :: forall a. ToResolver Error a GqlAff => a -> String -> Aff (Either GqlError (Result String))
resolveTyped resolver query = toAff mockRequest $ map (map message) <$> resolveTypedFiber resolver query
  where
  mockRequest :: Request
  mockRequest = unsafeCoerce unit