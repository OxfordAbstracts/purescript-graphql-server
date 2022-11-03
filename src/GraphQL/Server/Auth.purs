module GraphQL.Server.Auth where

import Prelude

data Auth m r =
  Auth (m Boolean) r
