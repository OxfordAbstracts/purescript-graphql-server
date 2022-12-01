module GraphQL.Server.CoerceVars where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Argonaut (Json)
import Data.Either (Either)
import Data.Foldable (foldM)
import Data.GraphQL.AST as AST
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Foreign.Object (Object)
import Foreign.Object as Object
import GraphQL.Server.EncodeValue (encodeValue)
import GraphQL.Server.GqlError (VariableInputError(..))
import GraphQL.Server.Introspection (Introspection_T)
import GraphQL.Server.Introspection.Types (ITypeKind(..))


coerceVars :: Introspection_T -> Maybe AST.VariableDefinitions -> Object Json -> Either VariableInputError (Object Json)
coerceVars _ Nothing _ = pure Object.empty
coerceVars introspection (Just (AST.VariableDefinitions varDefs)) rawVars = foldM go Object.empty varDefs
  where
  go :: Object Json -> AST.VariableDefinition -> Either VariableInputError (Object Json)
  go
    result
    ( AST.VariableDefinition
        { variable: AST.Variable variable
        , type: tipe
        , defaultValue
        }
    ) = do
    case isInputType tipe of
      Just false -> throwError $ VariableIsNotInputType variable
      Nothing -> throwError $ VariableTypeNotFound variable
      _ -> pure unit
    let
      hasValue = Object.member variable rawVars
      value = Object.lookup variable rawVars

    case hasValue, value, tipe, defaultValue of
      false, _, _, (Just (AST.DefaultValue default)) ->
        pure $ Object.insert variable (encodeValue result default) result
      _, Just val, _, _ ->
        pure $ Object.insert variable val result
      _, _, AST.Type_NonNullType _, _ ->
        throwError $ VariableHasNoValue variable
      _, _, _, _ -> pure result

  isInputType = case _ of
    AST.Type_ListType (AST.ListType t) -> isInputType t
    AST.Type_NonNullType (AST.NonNullType_ListType (AST.ListType t)) -> isInputType t
    AST.Type_NonNullType (AST.NonNullType_NamedType t) -> isInputNamedType t
    AST.Type_NamedType t -> isInputNamedType t

  isInputNamedType (AST.NamedType name) =
    lookupType name <#> \t -> (eq SCALAR || eq INPUT_OBJECT) t.kind

  lookupType name = introspection.__type { name } <#> unwrap

