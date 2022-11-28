module GraphQL.Server.Resolver.EncodeValue where

import Prelude

import Data.Argonaut (Json, encodeJson, fromObject, jsonNull)
import Data.GraphQL.AST as AST
import Data.List (List, foldl)
import Foreign.Object (Object)
import Foreign.Object as Object

encodeArguments :: Object Json -> List AST.Argument -> Json
encodeArguments vars = fromObject <<< foldl insertArg Object.empty
  where
  insertArg obj (AST.Argument { name, value }) = Object.insert name (encodeValue vars value) obj

encodeValue :: Object Json -> AST.Value -> Json
encodeValue vars = case _ of
  AST.Value_Variable (AST.Variable varName) -> Object.lookup varName vars # encodeJson
  AST.Value_IntValue (AST.IntValue a) -> encodeJson a
  AST.Value_FloatValue (AST.FloatValue a) -> encodeJson a
  AST.Value_StringValue (AST.StringValue a) -> encodeJson a
  AST.Value_BooleanValue (AST.BooleanValue a) -> encodeJson a
  AST.Value_NullValue _ -> jsonNull
  AST.Value_EnumValue (AST.EnumValue a) -> encodeJson a
  AST.Value_ListValue (AST.ListValue l) -> encodeJson $ map (encodeValue vars) l
  AST.Value_ObjectValue (AST.ObjectValue args) -> encodeArguments vars args