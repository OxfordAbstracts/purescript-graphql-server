module GraphQL.Server.Introspection.GetTypes where

import Prelude

import Data.List (List, foldl, (:))
import Data.Maybe (Maybe(..))
import Data.Typelevel.Num (toInt')
import GraphQL.Server.MaxDepth (maxDepth)
import GraphQL.Server.Introspection.Types (IField(..), IInputValue(..), IType(..))

getDescendantITypes :: IType -> List IType
getDescendantITypes = go depthLimit mempty
  where
  depthLimit = toInt' maxDepth

  go :: Int -> ITypes -> IType -> ITypes
  go 0 result _ = result
  go n result (IType t) =
    IType t : result
      # insertOfType
      # getFieldsITypes

    where
    insertOfType r = case t.ofType of
      Nothing -> r
      Just ofType -> go n r ofType

    getFieldsITypes r = case t.fields { includeDeprecated: Just true } of
      Just fields -> foldl (getFieldITypes (n - 1)) r fields
      _ -> r

  getFieldITypes :: Int -> ITypes -> IField -> ITypes
  getFieldITypes n result (IField f) =
    go n result f.type
      # \r -> foldl (getInputValueITypes n) r f.args

  getInputValueITypes :: Int -> ITypes -> IInputValue -> ITypes
  getInputValueITypes n result (IInputValue i) =
    go n result i.type

type ITypes = List IType