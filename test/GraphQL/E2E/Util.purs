module Test.GraphQL.E2E.Util where

import Prelude

import Affjax.Node (post, printError)
import Affjax.RequestBody (RequestBody(..))
import Affjax.ResponseFormat (json)
import Control.Monad.Error.Class (class MonadError)
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson, printJsonDecodeError, stringifyWithIndent)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmpty
import Data.Bifunctor (lmap)
import Data.Either (either)
import Data.Maybe (Maybe(..), isNothing, maybe)
import Data.Newtype (class Newtype)
import Effect.Aff (Aff, Error, error, throwError)
import Test.Spec.Assertions (shouldEqual)

shouldHaveData :: forall j. EncodeJson j => GqlRes -> j -> Aff Unit
shouldHaveData res json = res.data `shouldEqual` JsonTest (encodeJson json)

shouldHaveErrors :: forall j. EncodeJson j => GqlRes -> Array j -> Aff Unit
shouldHaveErrors res errors = maybe [] NonEmpty.toArray res.errors `shouldEqual` map (JsonTest <<< encodeJson) errors

noErrors :: GqlRes -> Aff Unit
noErrors res = isNothing res.errors `shouldEqual` true

gqlReq :: String -> Aff GqlRes
gqlReq operation = gqlReq_
  { operation
  }

gqlReqVars :: forall v. EncodeJson v => String -> v -> Aff GqlRes
gqlReqVars operation variables = gqlReq_
  { operation
  , variables
  }

gqlReq_ :: forall b. EncodeJson b => b -> Aff GqlRes
gqlReq_ body =
  post json "http://0.0.0.0:9000"
    ( Just $ Json $ encodeJson body
    )
    >>= either (printError >>> error >>> throwError) (_.body >>> pure)
    >>= decodeGql

type GqlRes = { data :: JsonTest, errors :: Maybe (NonEmptyArray JsonTest) }

-- | Json wrapper for tests with a show instance
-- | This allows it to be used in test assertions
newtype JsonTest = JsonTest Json

derive instance Newtype JsonTest _
derive instance Eq JsonTest
instance Show JsonTest where
  show (JsonTest json) = stringifyWithIndent 2 json

instance DecodeJson JsonTest where
  decodeJson json = JsonTest <$> decodeJson json

decodeGql :: forall m. MonadError Error m => Json -> m GqlRes
decodeGql = decodeJson >>> lmap (printJsonDecodeError >>> error) >>> either throwError pure

