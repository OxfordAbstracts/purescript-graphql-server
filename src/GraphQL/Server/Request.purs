module GraphQL.Server.Request where

import Data.Newtype (class Newtype)
import HTTPure (Request)

newtype GqlRequest = GqlRequest Request

derive instance Newtype GqlRequest _