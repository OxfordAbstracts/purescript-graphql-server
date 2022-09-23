module GraphQL.Resolver.HandleQuery where

import Prelude

import Data.Argonaut (Json)
import Data.Maybe (Maybe(..))
import GraphQL.Parser.AST as AST
import GraphQL.Resolver.JsonResolver (Resolver, resolve)
import GraphQL.Resolver.ToResolver (class ToJsonResolver)
import Unsafe.Coerce (unsafeCoerce)


handleOperationDefinition :: forall m. Applicative m =>  Resolver m  -> AST.OperationDefinition -> m Json
handleOperationDefinition resolver = case _ of
  AST.OperationDefinition_SelectionSet selectionSet -> resolve resolver (Just selectionSet)
  AST.OperationDefinition_OperationType _ -> unsafeCoerce unit

-- handleQuery :: forall m a . Resolver m -> String -> m Json
-- handleQuery resolver query = do
--   result <- runResolver resolver query
--   case result of
--     Left err -> pure $ encodeJson err
--     Right json -> pure json