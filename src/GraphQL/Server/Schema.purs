module GraphQL.Server.Schema where

import Prelude

data Schema :: forall k. k -> Type
data Schema s = Schema

schema
  :: Schema
       { query :: Query
       }
schema = Schema

type Query =
  { hello :: String
  }