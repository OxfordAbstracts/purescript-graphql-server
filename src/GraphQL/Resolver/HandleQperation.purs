module GraphQL.Resolver.HandleOperation where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Argonaut (Json)
import Data.Either (Either(..))
import Data.Foldable (foldM)
import Data.GraphQL.AST (OperationType(..))
import Data.GraphQL.AST as AST
import Data.List.NonEmpty as NonEmpty
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Foreign.Object (Object)
import Foreign.Object as Object
import GraphQL.Resolver (RootResolver)
import GraphQL.Resolver.EncodeValue (encodeValue)
import GraphQL.Resolver.GqlM (GqlM)
import GraphQL.Resolver.JsonResolver (resolve)
import GraphQL.Resolver.Result (encodeLocatedError, getLocatedErrors, resultToData)
import GraphQL.Server.GqlError (GqlError(..), VariableInputError(..))
import GraphQL.Server.Schema.Introspection (Introspection(..))
import GraphQL.Server.Schema.Introspection.Types (ITypeKind(..))
import HTTPure (Request)

handleOperation
  :: RootResolver 
  -> Request
  -> AST.OperationDefinition
  -> Object Json
  -> GqlM
       ( Either GqlError
           { data :: Json
           , errors :: Maybe (NonEmptyList Json)
           }
       )
handleOperation  root@{ introspection: Introspection introspection } request opDef rawVars = do
  case opDef of
    AST.OperationDefinition_SelectionSet selectionSet ->
      resolveToJson Object.empty query selectionSet
    AST.OperationDefinition_OperationType
      { operationType
      , variableDefinitions
      , selectionSet
      } ->
      case coerceVars variableDefinitions of
        Left err -> pure $ Left $ VariableInputError err
        Right vars -> do
          case operationType of
            Query -> resolveToJson vars query selectionSet
            Mutation -> resolveToJson vars mutation selectionSet
            Subscription -> pure $ Left $ SubscriptionsNotSupported
  where
  mutation = root.mutation request
  query = root.query request
  resolveToJson vars r selectionSet = getJson <$> resolve r vars (Just selectionSet)

  getJson result = pure
    { data: resultToData result
    , errors: NonEmpty.fromList $ encodeLocatedError <$> getLocatedErrors result
    }

  coerceVars :: Maybe AST.VariableDefinitions -> Either VariableInputError (Object Json)
  coerceVars Nothing = pure Object.empty
  coerceVars (Just (AST.VariableDefinitions varDefs)) = foldM go Object.empty varDefs
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

