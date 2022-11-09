module GraphQL.Server.Schema.Scalar where

import Data.Argonaut (Json, JsonDecodeError, decodeJson, encodeJson)
import Data.Date (Date)
import Data.DateTime (DateTime(..))
import Data.Either (Either)
import Data.Time (Time(..))
import GraphQL.Server.DateTime (decodeDate, decodeDateTime, decodeTime, encodeDate, encodeDateTime, encodeTime)

class Scalar :: Type -> Symbol -> Constraint
class
  Scalar a name
  | a -> name
  where
  encodeScalar :: a -> Json
  -- decodeScalar :: Json -> Either JsonDecodeError a


instance Scalar Boolean "Boolean" where 
  encodeScalar = encodeJson
  -- decodeScalar = decodeJson

instance Scalar Int "Int" where 
  encodeScalar = encodeJson
  -- decodeScalar = decodeJson

instance Scalar Number "Float" where 
  encodeScalar = encodeJson
  -- decodeScalar = decodeJson

instance Scalar String "String" where 
  encodeScalar = encodeJson
  -- decodeScalar = decodeJson

  
instance Scalar Date "Date" where 
  encodeScalar = encodeDate
  -- decodeScalar = decodeDate
  
instance Scalar Time "Time" where 
  encodeScalar = encodeTime
  -- decodeScalar = decodeTime

instance Scalar DateTime "DateTime" where 
  encodeScalar = encodeDateTime
  -- decodeScalar = decodeDateTime