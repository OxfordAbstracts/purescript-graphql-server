module GraphQL.Server.Value.Decode where

import Prelude

import Data.Argonaut (Json, JsonDecodeError)
import Data.Either (Either)
import Data.GraphQL.AST as AST



-- decodeValue :: AST.Type -> Json -> Either JsonDecodeError AST.Value
-- decodeValue t json = case t of 
  