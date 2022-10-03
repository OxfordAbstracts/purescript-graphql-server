module GraphQL.Server
  ( start
  ) where

import Prelude

import Data.Argonaut (Json)
import Data.Either (Either)
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import GraphQL.Resolver.GqlSequential (class GqlSequential)
import GraphQL.Resolver.JsonResolver (Resolver)
import GraphQL.Resolver.ToResolver (class ToResolver, toResolver)
import GraphQL.Server.GqlError (GqlError)
import GraphQL.Server.GqlResM (toResponse)
import GraphQL.Server.HandleRequest (handleRequest)
import GraphQL.Server.Schema (GqlRoot)
import HTTPure (ServerM, serve)

-- | Boot up the server
start
  :: forall query m f
   . GqlSequential f m
  => ToResolver (GqlRoot query Unit) f
  => { root :: GqlRoot query Unit
     , runM :: f (Either GqlError Json) -> Aff (Either GqlError Json)
     }
  -> ServerM
start { root, runM } = serve port (handleRequest runM resolvers >>> toResponse) onStart
  where
  resolvers :: Resolver f
  resolvers = toResolver root

  port = 9000
  onStart = do
    log $ "Graphql server listening at http://0.0.0.0:" <> show port

