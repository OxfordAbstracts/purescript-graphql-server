module GraphQL.Resolver.Result where

import Prelude

import Data.Argonaut (Json, encodeJson, fromArray, fromObject, jsonNull, stringify)
import Data.Array as Array
import Data.Either (Either(..), either)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Lazy (Lazy, force)
import Data.List (List(..), fold, reverse, (:))
import Data.Maybe (Maybe, maybe)
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import GraphQL.Server.GqlError (ResolverError)

data Result
  = ResultLeaf Json
  | ResultLazy (Lazy Result)
  | ResultError ResolverError
  | ResultObject (List (Tuple String Result))
  | ResultList (List Result)
  | ResultNullable (Maybe Result)

derive instance Generic Result _
derive instance Eq Result

instance Show Result where
  show = case _ of
    ResultLeaf json -> "(ResultLeaf " <> stringify json <> ")"
    ResultLazy json -> "(ResultLazy " <> show (force json) <> ")"
    ResultError err -> "(ResultError " <> show err <> ")"
    ResultObject fields -> "(ResultObject " <> show fields <> ")"
    ResultList items -> "(ResultList " <> show items <> ")"
    ResultNullable maybeResult -> "(ResultNullable " <> show maybeResult <> ")"

resultToData :: Result -> Json
resultToData = case _ of
  ResultLeaf json -> json
  ResultError _err -> jsonNull
  ResultLazy res -> resultToData (force res)
  ResultObject fields -> fromObject $ Object.fromFoldable $
    fields <#> map resultToData
  ResultList items -> fromArray $ Array.fromFoldable $ map resultToData items
  ResultNullable maybeResult -> maybe jsonNull resultToData maybeResult

newtype LocatedError = LocatedError
  { path :: List (Either Int String)
  , message :: String
  , locations :: List { line :: Int, column :: Int }
  }

getLocatedErrors :: Result -> List LocatedError
getLocatedErrors = go Nil
  where
  go :: List (Either Int String) -> Result -> List LocatedError
  go path = case _ of
    ResultLeaf _ -> Nil
    ResultLazy res -> go path $ force res
    ResultError err -> pure $ LocatedError
      { path: reverse path
      , message: show err
      , locations: Nil -- TODO
      }

    ResultObject fields -> fields
      >>= \(Tuple name result) -> go (Right name : path) result

    ResultList items -> items
      # mapWithIndex (\i -> go (Left i : path))
      # fold

    ResultNullable maybeResult -> maybe Nil (go path) maybeResult

encodeLocatedError :: LocatedError -> Json
encodeLocatedError (LocatedError { path, message, locations }) =
  encodeJson
    { path: either encodeJson encodeJson <$> path
    , message
    , locations
    }