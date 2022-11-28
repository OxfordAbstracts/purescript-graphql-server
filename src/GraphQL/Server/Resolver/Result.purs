module GraphQL.Server.Resolver.Result where

import Prelude

import Data.Argonaut (Json, encodeJson, fromArray, fromObject, jsonNull, stringify)
import Data.Array as Array
import Data.FunctorWithIndex (mapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), fold, reverse, (:))
import Data.Tuple (Tuple(..))
import Effect.Exception (Error, message)
import Foreign.Object as Object
import GraphQL.Server.Resolver.Path (Path, PathPart(..), encodePath)
import GraphQL.Server.GqlError (FailedToResolve)

data Result err
  = ResultLeaf Json
  | ResultError (FailedToResolve err)
  | ResultObject (List (Tuple String (Result err)))
  | ResultList (List (Result err))
  | ResultNull

derive instance Generic (Result err) _
derive instance Functor Result
derive instance Eq err => Eq (Result err)

instance Show err => Show (Result err) where
  show = case _ of
    ResultLeaf json -> "(ResultLeaf " <> stringify json <> ")"
    ResultError err -> "(ResultError " <> show err <> ")"
    ResultObject fields -> "(ResultObject " <> show fields <> ")"
    ResultList items -> "(ResultList " <> show items <> ")"
    ResultNull -> "ResultNull"

resultToData :: forall err. Result err -> Json
resultToData = case _ of
  ResultLeaf json -> json
  ResultError _err -> jsonNull
  ResultObject fields -> fromObject $ Object.fromFoldable $
    fields <#> map resultToData
  ResultList items -> fromArray $ Array.fromFoldable $ map resultToData items
  ResultNull -> jsonNull

newtype LocatedError = LocatedError
  { path :: Path
  , message :: String
  , locations :: List { line :: Int, column :: Int }
  }

getLocatedErrors :: Result Error -> List LocatedError
getLocatedErrors = go Nil
  where
  go :: Path -> Result Error -> List LocatedError
  go path = case _ of
    ResultLeaf _ -> Nil
    ResultError err -> pure $ LocatedError
      { path: reverse path
      , message: show $ map message err
      , locations: Nil
      }

    ResultObject fields -> fields
      >>= \(Tuple name result) -> go (Field name : path) result

    ResultList items -> items
      # mapWithIndex (\i -> go (Index i : path))
      # fold

    ResultNull -> Nil

encodeLocatedError :: LocatedError -> Json
encodeLocatedError (LocatedError { path, message, locations }) =
  encodeJson
    { path: encodePath path
    , message
    , locations
    }