module GraphQL.Server.Util.Coerce (CoerceError, coerceValue) where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, JsonDecodeError(..), decodeJson, encodeJson)
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.GraphQL.AST as AST
import Data.Maybe (Maybe(..))
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

data CoerceError
  = NullValueAtNonNullType
  | TypeMismatch String String
  | UnrecognizedNamedType String
  | CoerceJsonDecodeError JsonDecodeError
  | NotYetSupported String

-- | coerce and type check value against a type
coerceValue :: Json -> Maybe AST.Value -> AST.Type -> Either CoerceError Json
coerceValue json val = case _ of
  AST.Type_NamedType (AST.NamedType name) -> case name of
    "String" -> typeCheck (Proxy :: Proxy String) json
    "Int" -> typeCheck (Proxy :: Proxy Int) json
    "Float" -> typeCheck (Proxy :: Proxy Number) json
    "Boolean" -> typeCheck (Proxy :: Proxy Boolean) json
    "ID" -> typeCheck (Proxy :: Proxy String) json
    _ -> Right json
  _ -> Left $ UnrecognizedNamedType $ "?d json"

typeCheck :: forall a. DecodeJson a => EncodeJson a => Proxy a -> Json -> Either CoerceError Json
typeCheck proxy = typeCheckNotNull (map Just proxy)

typeCheckNotNull :: forall a. DecodeJson a => EncodeJson a => Proxy a -> Json -> Either CoerceError Json
typeCheckNotNull _ = decodeJson >>> bimap CoerceJsonDecodeError (encodeJson :: a -> _)

-- AST.Type_NonNullType _, AST.Value_NullValue _-> Left NullValueAtNonNullType
-- AST.Type_NamedType t, v@(AST.Value_NullValue _)-> Right v
-- AST.Type_NamedType (AST.NamedType "Boolean"), v@(AST.Value_BooleanValue (AST.BooleanValue s)) -> Right v
-- AST.Type_NamedType (AST.NamedType "Boolean"), _ -> Right v
-- AST.Type_NamedType (AST.NamedType "Int"), v@(AST.Value_IntValue (AST.IntValue s)) -> Right v
-- AST.Type_NamedType (AST.NamedType "Float"), v@(AST.Value_FloatValue (AST.FloatValue s)) -> Right v
-- AST.Type_NamedType (AST.NamedType "String"), v@(AST.Value_StringValue (AST.StringValue s)) -> Right v
-- AST.Type_NamedType (AST.NamedType "ID"), v@(AST.Value_StringValue (AST.StringValue s)) -> Right v
-- AST.Type_NamedType (AST.NamedType "ID"), v@(AST.Value_IntValue (AST.IntValue s)) -> Right v
-- AST.Type_NamedType (AST.NamedType _), v@(AST.Value_StringValue (AST.StringValue s)) -> Right v

-- AST.NamedType (AST.NamedType "String") (AST.ValueString s) -> Right $ AST.String s
-- AST.NamedType (AST.NamedType "Int") (AST.ValueInt i) -> Right $ AST.Int i
-- AST.NamedType (AST.NamedType "Float") (AST.ValueFloat f) -> Right $ AST.Float f
-- AST.TypeNamed (AST.NamedType "Boolean") (AST.ValueBoolean b) -> Right $ AST.Boolean b
-- AST.TypeNamed (AST.NamedType "ID") (AST.ValueString s) -> Right $ AST.String s
-- AST.TypeNamed (AST.NamedType "ID") (AST.ValueInt i) -> Right $ AST.Int i
-- AST.TypeNamed (AST.NamedType "ID") (AST.ValueFloat f) -> Right $ AST.Float f
-- AST.TypeNamed (AST.NamedType "ID") (AST.ValueBoolean b) -> Right $ AST.Boolean b
-- AST.TypeNamed (AST.NamedType "ID") (AST.ValueNull) -> Right $ AST.Null
-- AST.TypeNamed (AST.NamedType "ID") (AST.c e) -> Right $ AST.Enum e
-- AST.TypeNamed (AST.NamedType "ID") (AST.ValueList l) -> Right $ AST.List l
-- AST.TypeNamed (AST.NamedType "ID") (AST.ValueObject o) -> Right $ AST.Object o
-- AST.TypeNamed (AST.NamedType "ID") (AST.ValueVariable v) -> Right $ AST.Variable v
-- AST.TypeNamed (AST.NamedType "ID") (AST.ValueInputObject o) -> Right $ AST.InputObject o
-- AST.TypeNamed (AST.NamedType "ID") (AST.ValueDirective d) -> Right $ AST.Directive d
-- AST.TypeNamed (AST.NamedType "ID") (AST.ValueFragmentSpread f) -> Right $ AST.FragmentSpread f
-- AST.TypeNamed (AST.NamedType "ID") (AST.ValueInlineFragment f) -> Right $ AST.InlineFragment f
-- AST.TypeNamed (AST.NamedType "ID") (AST.ValueFragmentDefinition f) -> Right $ AST.FragmentDefinition f
-- AST.TypeNamed (AST.NamedType "ID") (AST.ValueOperationDefinition o) -> Right $ AST.OperationDefinition o
-- AST.TypeNamed (AST.NamedType "ID") (AST.ValueSchemaDefinition s) -> Right $ AST.SchemaDefinition s
