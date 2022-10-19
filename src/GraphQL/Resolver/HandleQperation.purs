module GraphQL.Resolver.HandleOperation where

import Prelude

import Data.Argonaut (Json)
import Data.Either (Either(..))
import Data.GraphQL.AST (OperationType(..))
import Data.GraphQL.AST as AST
import Data.List.NonEmpty as NonEmpty
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..))
import Foreign.Object (Object)
import GraphQL.Resolver (RootResolver)
import GraphQL.Resolver.Gqlable (class Gqlable)
import GraphQL.Resolver.JsonResolver (resolve)
import GraphQL.Resolver.Result (encodeLocatedError, getLocatedErrors, resultToData)
import GraphQL.Server.GqlError (GqlError(..), VariableInputError)

handleOperation
  :: forall m f
   . Gqlable f m
  => RootResolver f
  -> AST.OperationDefinition
  -> Object Json
  -> f
       ( Either GqlError
           { data :: Json
           , errors :: Maybe (NonEmptyList Json)
           }
       )
handleOperation { mutation, query } opDef rawVars = do
  case coerceVars rawVars of
    Left err -> pure $ Left $ VariableInputError err
    Right vars -> do
      case opDef of
        AST.OperationDefinition_SelectionSet selectionSet ->
          resolveToJson query selectionSet
        AST.OperationDefinition_OperationType
          { operationType
          -- , variableDefinitions --  ∷ (Maybe VariableDefinitions)
          -- , directives --  ∷ (Maybe Directives)
          , selectionSet
          } -> case operationType of
          Query -> resolveToJson query selectionSet
          Mutation -> resolveToJson mutation selectionSet
          Subscription -> pure $ Left $ SubscriptionsNotSupported
      where
      resolveToJson r selectionSet = getJson <$> resolve r vars (Just selectionSet)

  where
  getJson result = pure
    { data: resultToData result
    , errors: NonEmpty.fromList $ encodeLocatedError <$> getLocatedErrors result
    }

  coerceVars :: Object Json -> Either VariableInputError (Object Json)
  coerceVars = pure -- TODO: implement

-- makeVariables :: Object Json -> List AST.VariableDefinition -> Either VariableInputError (Object Json)
-- makeVariables vars = foldM makeVar Object.empty
-- where
-- makeVar :: Object Json -> AST.VariableDefinition -> Either VariableInputError (Object Json)
-- makeVar coercedVars (AST.VariableDefinition varDef@{ variable: AST.Variable varName }) =
--   case Object.lookup varName vars, varDef of

--     Just var, _ -> Right $ Object.insert varName var coercedVars

--     Nothing, { defaultValue: Just (AST.DefaultValue defaultValue) } ->
--       Right
--         $ Object.insert varName (encodeValue defaultValue) coercedVars

--     _, _ -> Left $ VariableNotFound varName

