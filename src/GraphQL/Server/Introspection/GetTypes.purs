module GraphQL.Server.Introspection.GetTypes where

import Prelude

import Data.List (List, foldl, (:))
import Data.Maybe (Maybe(..))
import Data.Typelevel.Num (toInt')
import Effect.Aff (Aff)
import GraphQL.Server.Introspection.Types (IField(..), IInputValue(..), IType(..))
import GraphQL.Server.MaxDepth (maxDepth)

getDescendantITypes :: IType -> Aff ITypes
getDescendantITypes =
  go depthLimit (pure mempty)
  where
  depthLimit = toInt' maxDepth

  go :: Int -> Aff ITypes -> IType -> Aff ITypes
  go 0 result _ = result
  go n result (IType t) = do
    result' <- result
    pure (IType t : result')
      # insertOfType
      # getFieldsITypes

    where
    insertOfType :: Aff ITypes -> Aff ITypes
    insertOfType r = case t.ofType of
      Nothing -> r
      Just ofType -> go n r ofType

    getFieldsITypes :: Aff ITypes -> Aff ITypes
    getFieldsITypes r = do
      fields <- t.fields { includeDeprecated: Just true }
      case fields of
        Just fields' -> foldl (getFieldITypes (n - 1)) r fields'
        _ -> r

  getFieldITypes :: Int -> Aff ITypes -> IField -> Aff ITypes
  getFieldITypes n result (IField f) =
    go n result f.type
      # \r -> foldl (getInputValueITypes n) r f.args

  getInputValueITypes :: Int -> Aff ITypes -> IInputValue -> Aff ITypes
  getInputValueITypes n result (IInputValue i) =
    go n result i.type

type ITypes = List IType