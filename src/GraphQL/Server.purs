module GraphQL.Server
  ( start
  ) where

import Prelude

import Effect.Class.Console (log)
import GraphQL.Resolver (RootResolver, rootResolver)
import GraphQL.Resolver.Gqlable (class Gqlable)
import GraphQL.Resolver.JsonResolver (Resolver)
import GraphQL.Resolver.Root (GqlRoot, MutationRoot, QueryRoot)
import GraphQL.Resolver.ToResolver (class ToResolver, toResolver)
import GraphQL.Server.GqlResM (toResponse)
import GraphQL.Server.HandleRequest (handleRequest)
import GraphQL.Server.Schema (class GetSchema)
import GraphQL.Server.Schema.Introspection (IntrospectionRow)
import HTTPure (ServerM, serve)
import Prim.Row (class Nub)
import Type.Proxy (Proxy(..))

-- | Boot up the server
start
  :: forall query mutation withIntrospection m f
   . Gqlable f m
  => Nub (IntrospectionRow query) withIntrospection
  => ToResolver ((QueryRoot { | withIntrospection })) f
  => ToResolver (MutationRoot mutation) f
  => GetSchema (GqlRoot (QueryRoot { | query }) (MutationRoot mutation))
  => { root :: { query :: { | query }, mutation :: mutation }
     , runsOn :: Proxy (f Unit)
     }
  -> ServerM
start { root } = serve port (handleRequest resolvers >>> toResponse) onStart
  where
  resolvers :: RootResolver f
  resolvers = rootResolver root

  port = 9000
  onStart = do
    log $ "Graphql server listening at http://0.0.0.0:" <> show port
