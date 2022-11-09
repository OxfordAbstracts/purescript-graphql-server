module GraphQL.Server.Decode where

import Prelude

import Data.Argonaut (Json, JsonDecodeError(..), decodeJson, toObject)
import Data.Argonaut.Decode.Decoders (decodeArray, decodeCodePoint, decodeForeignObject, decodeList, decodeMap, decodeMaybe, decodeSet, decodeVoid)
import Data.Bifunctor (lmap)
import Data.Date (Date)
import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.List (List)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Set as S
import Data.String (CodePoint)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Time (Time)
import Foreign.Object as FO
import GraphQL.Server.DateTime (decodeDate, decodeDateTime, decodeTime)
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Type.Proxy (Proxy(..))

class DecodeArg a where
  decodeArg :: Json -> Either JsonDecodeError a

instance DecodeArg Boolean where
  decodeArg = decodeJson

instance DecodeArg Int where
  decodeArg = decodeJson

instance DecodeArg Number where
  decodeArg = decodeJson

instance DecodeArg String where
  decodeArg = decodeJson

instance DecodeArg Date where
  decodeArg = decodeDate

instance DecodeArg Time where
  decodeArg = decodeTime

instance DecodeArg DateTime where
  decodeArg = decodeDateTime

instance DecodeArg a => DecodeArg (Array a) where
  decodeArg = decodeArray decodeArg

instance DecodeArg a => DecodeArg (Maybe a) where
  decodeArg = decodeMaybe decodeArg

instance DecodeArg CodePoint where
  decodeArg = decodeCodePoint

instance DecodeArg a => DecodeArg (FO.Object a) where
  decodeArg = decodeForeignObject decodeArg

instance DecodeArg a => DecodeArg (List a) where
  decodeArg = decodeList decodeArg

instance (Ord a, DecodeArg a) => DecodeArg (S.Set a) where
  decodeArg = decodeSet decodeArg

instance (Ord a, DecodeArg a, DecodeArg b) => DecodeArg (M.Map a b) where
  decodeArg = decodeMap decodeArg decodeArg

instance DecodeArg Void where
  decodeArg = decodeVoid

instance decodeRecord ::
  ( GDecodeArg row list
  , RL.RowToList row list
  ) =>
  DecodeArg (Record row) where
  decodeArg json =
    case toObject json of
      Just object -> gDecodeArg object (Proxy :: Proxy list)
      Nothing -> Left $ TypeMismatch "Object"

class GDecodeArg (row :: Row Type) (list :: RL.RowList Type) | list -> row where
  gDecodeArg :: forall proxy. FO.Object Json -> proxy list -> Either JsonDecodeError (Record row)

instance gDecodeArgNil :: GDecodeArg () RL.Nil where
  gDecodeArg _ _ = Right {}

instance gDecodeArgCons ::
  ( DecodeArgField value
  , GDecodeArg rowTail tail
  , IsSymbol field
  , Row.Cons field value rowTail row
  , Row.Lacks field rowTail
  ) =>
  GDecodeArg row (RL.Cons field value tail) where
  gDecodeArg object _ = do
    let
      _field = Proxy :: Proxy field
      fieldName = reflectSymbol _field
      fieldValue = FO.lookup fieldName object

    case decodeArgField fieldValue of
      Just fieldVal -> do
        val <- lmap (AtKey fieldName) fieldVal
        rest <- gDecodeArg object (Proxy :: Proxy tail)
        Right $ Record.insert _field val rest

      Nothing ->
        Left $ AtKey fieldName MissingValue

class DecodeArgField a where
  decodeArgField :: Maybe Json -> Maybe (Either JsonDecodeError a)

instance decodeFieldMaybe ::
  DecodeArg a =>
  DecodeArgField (Maybe a) where
  decodeArgField Nothing = Just $ Right Nothing
  decodeArgField (Just j) = Just $ decodeArg j

else instance decodeFieldId ::
  DecodeArg a =>
  DecodeArgField a where
  decodeArgField j = decodeArg <$> j
