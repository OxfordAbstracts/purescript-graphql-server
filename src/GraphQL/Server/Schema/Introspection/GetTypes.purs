module GraphQL.Server.Schema.Introspection.GetTypes where

import Prelude

import Data.Foldable (length)
import Data.List (List, any, foldl, (:))
import Data.List.NonEmpty as NonEmpty
import Data.List.Types (NonEmptyList)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Typelevel.Num (toInt')
import GraphQL.Server.Schema.Introspection.GetType (DepthLimit)
import GraphQL.Server.Schema.Introspection.Types (IField(..), IInputValue(..), IType(..))
import Type.Proxy (Proxy(..))

-- hasDuplicateTypes :: IType -> Boolean
-- hasDuplicateTypes = getDescendantITypes >>> any (\ts -> length ts > 1)

-- unlistTypes :: TypeLists -> Map String IType
-- unlistTypes = map NonEmpty.head

getDescendantITypes :: IType -> TypeLists
getDescendantITypes = go depthLimit mempty
  where
  depthLimit = toInt' (Proxy :: Proxy DepthLimit)

  go :: Int -> TypeLists -> IType -> TypeLists
  go 0 result _ = result
  go n result (IType t) =
    result
      # insertSelf
      # insertOfType
      # getFieldsITypes

    where
    insertSelf = (:) (IType t)
    insertOfType r = case t.ofType of
      Nothing -> r
      Just ofType -> go n r ofType

    getFieldsITypes r = case t.fields { includeDeprecated: Just true } of
      Just fields -> foldl (getFieldITypes (n - 1)) r fields
      _ -> r

  getFieldITypes :: Int -> TypeLists -> IField -> TypeLists
  getFieldITypes n result (IField f) =
    go n result f.type
      # \r -> foldl (getInputValueITypes n) r f.args

  getInputValueITypes :: Int -> TypeLists -> IInputValue -> TypeLists
  getInputValueITypes n result (IInputValue i) =
    go n result i.type

type TypeLists = List IType