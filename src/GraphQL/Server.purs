module GraphQL.Server
  ( start
  ) where

import Prelude

import Effect.Class.Console (log)
import GraphQL.Server.GqlResM (toResponse)
import GraphQL.Server.HandleRequest (handleRequest)
import HTTPure (ServerM, serve)

-- | Boot up the server
start
  :: _
  -> ServerM
start opts = serve port (handleRequest >>> toResponse) onStart
  where
  port = 9000
  onStart = do
    log $ "Graphql server listening at http://0.0.0.0:" <> show port

  resolvers =
    { query: { hello: "world" }
    }

