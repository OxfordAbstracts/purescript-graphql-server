module GraphQL.Server.Value.Encode where

import Prelude

import Data.Argonaut (Json, encodeJson, fromObject, jsonNull)
import Data.GraphQL.AST (Value(..))
import Data.GraphQL.AST as AST
import Data.List (List)
import Data.Tuple (Tuple(..))
import Foreign.Object as Object

encodeValue :: Value -> Json
encodeValue = case _ of
  Value_Variable v -> encodeJson $ show v
  Value_IntValue (AST.IntValue v) -> encodeJson v
  Value_FloatValue (AST.FloatValue v) -> encodeJson v
  Value_StringValue (AST.StringValue v) -> encodeJson v
  Value_BooleanValue (AST.BooleanValue v) -> encodeJson v
  Value_NullValue AST.NullValue -> jsonNull
  Value_EnumValue (AST.EnumValue v) -> encodeJson v
  Value_ListValue (AST.ListValue v) -> encodeJson $ map encodeValue v
  Value_ObjectValue (AST.ObjectValue v) -> encodeArgs v

encodeArgs :: List AST.Argument -> Json
encodeArgs args = fromObject $ Object.fromFoldable
  $ args <#>
      \(AST.Argument { name, value }) -> Tuple name $ encodeValue value
