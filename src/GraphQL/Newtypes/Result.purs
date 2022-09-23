module GraphQL.Newtypes.Result where

import Prelude

import Data.Argonaut (Json, stringify)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
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