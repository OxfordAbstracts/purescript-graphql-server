module GraphQL.Server
  ( start
  ) where

import Prelude

import Effect.Class.Console (log)
import GraphQL.Resolver.Gqlable (class Gqlable)
import GraphQL.Resolver.JsonResolver (Resolver)
import GraphQL.Resolver.ToResolver (class ToResolver, toResolver)
import GraphQL.Server.GqlResM (toResponse)
import GraphQL.Server.HandleRequest (handleRequest)
import GraphQL.Server.Schema (GqlRoot)
import HTTPure (ServerM, serve)

-- | Boot up the server
start
  :: forall query m f
   . Gqlable f m
  => ToResolver (GqlRoot query Unit) f
  => { root :: GqlRoot query Unit
     }
  -> ServerM
start { root } = serve port (handleRequest resolvers >>> toResponse) onStart
  where
  resolvers :: Resolver f
  resolvers = toResolver root

  port = 9000
  onStart = do
    log $ "Graphql server listening at http://0.0.0.0:" <> show port

