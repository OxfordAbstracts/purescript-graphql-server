module GraphQL.Server
  ( start
  ) where

import Prelude

import Data.Argonaut (Json)
import Data.Either (Either)
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import GraphQL.Resolver.JsonResolver (Resolver)
import GraphQL.Resolver.ToResolver (class ToResolver, toResolver)
import GraphQL.Server.GqlError (GqlError)
import GraphQL.Server.GqlResM (toResponse)
import GraphQL.Server.HandleRequest (handleRequest)
import GraphQL.Server.Schema (GqlRoot)
import HTTPure (ServerM, serve)

-- | Boot up the server
start
  :: forall query m
   . Applicative m
  => ToResolver (GqlRoot query Unit) m
  => { root :: GqlRoot query Unit
     , runM :: m (Either GqlError Json) -> Aff (Either GqlError Json)
     }
  -> ServerM
start { root, runM } = serve port (handleRequest runM resolvers >>> toResponse) onStart
  where
  resolvers :: Resolver m
  resolvers = toResolver root

  port = 9000
  onStart = do
    log $ "Graphql server listening at http://0.0.0.0:" <> show port

