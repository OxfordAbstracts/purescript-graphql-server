module GraphQL.Server.HandleOperation where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader as H
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
import GraphQL.Server.EncodeValue (encodeValue)
import GraphQL.Server.GqlError (GqlError(..), VariableInputError(..))
import GraphQL.Server.GqlM (GqlM)
import GraphQL.Server.Introspection (Introspection(..), Introspection_T)
import GraphQL.Server.Introspection.Types (ITypeKind(..))
import GraphQL.Server.Resolver.JsonResolver (resolve)
import GraphQL.Server.Resolver.Result (encodeLocatedError, getLocatedErrors, resultToData)

handleOperation
  :: forall env
   . GqlM env (RootResolver env)
  -> AST.OperationDefinition
  -> Object Json
  -> GqlM env
       ( Either GqlError
           { data :: Json
           , errors :: Maybe (NonEmptyList Json)
           }
       )
handleOperation rootM opDef rawVars = do
  root@{ introspection: Introspection introspection } <- rootM
  { request } <- H.ask
  let
    mutation = root.mutation request
    query = root.query request
    vars' = case opDef of
      AST.OperationDefinition_SelectionSet _ ->
        Right Object.empty
      AST.OperationDefinition_OperationType
        { variableDefinitions
        } -> coerceVars introspection variableDefinitions

  case vars' of
    Left err -> pure $ Left $ VariableInputError err
    Right vars -> do
      case opDef of
        AST.OperationDefinition_SelectionSet selectionSet ->
          resolveToJson query selectionSet
        AST.OperationDefinition_OperationType
          { operationType
          , selectionSet
          } ->
          case operationType of
            Query -> resolveToJson query selectionSet
            Mutation -> resolveToJson mutation selectionSet
            Subscription -> pure $ Left $ SubscriptionsNotSupported
  where

  resolveToJson r selectionSet = getJson <$> resolve r (Just selectionSet)

  getJson result = pure
    { data: resultToData result
    , errors: NonEmpty.fromList $ encodeLocatedError <$> getLocatedErrors result
    }

  coerceVars :: Introspection_T -> Maybe AST.VariableDefinitions -> Either VariableInputError (Object Json)
  coerceVars _ Nothing = pure Object.empty
  coerceVars introspection (Just (AST.VariableDefinitions varDefs)) = foldM go Object.empty varDefs
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

