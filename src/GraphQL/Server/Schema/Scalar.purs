module GraphQL.Server.Schema.Scalar where

import Data.Argonaut (Json, JsonDecodeError)
import Data.Either (Either)

class Scalar :: Type -> Symbol -> Constraint
class
  Scalar a name
  | a -> name
  where
  encodeScalar :: a -> Json
  decodeScalar :: Json -> Either JsonDecodeError a
