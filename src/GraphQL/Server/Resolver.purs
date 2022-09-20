module GraphQL.Server.Resolver where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Argonaut (class EncodeJson, Json, encodeJson)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import GraphQL.Parser.AST as AST
import GraphQL.Server.GqlError (GqlError(..), ResolverError(..))
import GraphQL.Server.GqlResM (GqlResM)
import Unsafe.Coerce (unsafeCoerce)

class Resolver resolver where
  resolve :: (GqlResM resolver) -> Maybe AST.SelectionSet -> GqlResM Json

-- instance Resolver {|r} where 
--   resolve resolver = case _ of 
--     Nothing -> throwResolver MissingSelectionSet
--     (Just Nil) -> throwResolver EmptySelectionSet
--     Just sel -> 

-- else i
instance (EncodeJson a, NodeResolver a) => Resolver a where
  resolve resolver = case _ of
    Nothing -> map encodeJson resolver
    Just _ -> throwResolver SelectionSetAtNodeValue

throwResolver ∷ ∀ (t61 ∷ Type -> Type) (t62 ∷ Type). MonadThrow GqlError t61 ⇒ ResolverError → t61 t62
throwResolver = throwError <<< ResolverError

class NodeResolver :: forall k. k -> Constraint
class NodeResolver a

instance NodeResolver Boolean
instance NodeResolver Int
instance NodeResolver Number
instance NodeResolver String
instance NodeResolver a => NodeResolver (Maybe a)
instance NodeResolver a => NodeResolver (Array a)

getSelections :: AST.OperationDefinition -> List (AST.Selection)
getSelections = unwrap <<< case _ of
  AST.OperationDefinition_SelectionSet s -> s
  AST.OperationDefinition_OperationType { selectionSet } -> selectionSet

