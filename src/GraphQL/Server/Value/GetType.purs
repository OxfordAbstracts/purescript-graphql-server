module GraphQL.Server.Value.GetType where

import Prelude

import Data.GraphQL.AST as AST
import Data.List (head)
import Data.Maybe (Maybe(..))

getValueType :: AST.Value -> Maybe AST.Type
getValueType = case _ of
  AST.Value_Variable (AST.Variable _v) -> Nothing
  AST.Value_IntValue (AST.IntValue _v) -> Just $ AST.Type_NamedType $ AST.NamedType "Int"
  AST.Value_FloatValue (AST.FloatValue _v) -> Just $ AST.Type_NamedType $ AST.NamedType "Float"
  AST.Value_StringValue (AST.StringValue _v) -> Just $ AST.Type_NamedType $ AST.NamedType "String"
  AST.Value_BooleanValue (AST.BooleanValue _v) -> Just $ AST.Type_NamedType $ AST.NamedType "Boolean"
  AST.Value_NullValue AST.NullValue -> Nothing
  AST.Value_EnumValue (AST.EnumValue _v) -> Nothing
  AST.Value_ListValue (AST.ListValue v) ->
    AST.Type_ListType <$> AST.ListType <$> (getValueType =<< (head v))
  AST.Value_ObjectValue (AST.ObjectValue _v) -> Nothing