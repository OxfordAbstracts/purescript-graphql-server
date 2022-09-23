module GraphQL.Resolver.Result where

import Prelude

import Data.Argonaut (Json, fromArray, fromObject, jsonNull, stringify)
import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Maybe (Maybe, maybe)
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import GraphQL.Server.GqlError (ResolverError)

data Result
  = ResultLeaf Json
  | ResultError ResolverError
  | ResultObject (List (Tuple String Result))
  | ResultList (List Result)
  | ResultNullable (Maybe Result)

derive instance Generic Result _
derive instance Eq Result

instance Show Result where
  show = case _ of
    ResultLeaf json -> "(ResultLeaf " <> stringify json <> ")"
    ResultError err -> "(ResultError " <> show err <> ")"
    ResultObject fields -> "(ResultObject " <> show fields <> ")"
    ResultList items -> "(ResultList " <> show items <> ")"
    ResultNullable maybeResult -> "(ResultNullable " <> show maybeResult <> ")"

resultToData :: Result -> Json
resultToData = case _ of
  ResultLeaf json -> json
  ResultError _err -> jsonNull
  ResultObject fields -> fromObject $ Object.fromFoldable $
    fields <#> \(Tuple name result) -> Tuple name $ resultToData result
  ResultList items -> fromArray $ Array.fromFoldable $ map resultToData items
  ResultNullable maybeResult -> maybe jsonNull resultToData maybeResult

-- resultToError :: Result -> Array 