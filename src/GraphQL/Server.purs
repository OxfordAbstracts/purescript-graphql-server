module GraphQL.Server
  ( GqlServerM(..)
  , GqlServer
  , start
  ) where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)
import GraphQL.Resolver (RootResolver, rootResolver)
import GraphQL.Resolver.GqlIo (GqlIo)
import GraphQL.Resolver.Gqlable (class Gqlable)
import GraphQL.Resolver.Root (GqlRoot, MutationRoot, QueryRoot)
import GraphQL.Resolver.ToResolver (class ToResolver)
import GraphQL.Server.GqlResM (toResponse)
import GraphQL.Server.HandleRequest (handleRequest)
import GraphQL.Server.Schema (class GetSchema)
import GraphQL.Server.Schema.Introspection (IntrospectionRow)
import HTTPure (ServerM, serve)
import Prim.Row (class Nub)

-- | Boot up the server
start
  :: forall query mutation withIntrospection m f
   . Gqlable f m
  => Nub (IntrospectionRow query) withIntrospection
  => ToResolver ((QueryRoot { | withIntrospection })) f
  => ToResolver (MutationRoot mutation) f
  => GetSchema (GqlRoot (QueryRoot { | query }) (MutationRoot mutation))
  => { root :: { query :: { | query }, mutation :: mutation }
     }
  -> GqlServerM f
start { root } = GqlServerM $ serve port (handleRequest resolvers >>> toResponse) onStart
  where
  resolvers :: RootResolver f
  resolvers = rootResolver root

  port = 9000
  onStart = do
    log $ "Graphql server listening at http://0.0.0.0:" <> show port

newtype GqlServerM :: forall k. k -> Type
newtype GqlServerM f = GqlServerM ServerM

type GqlServer :: forall k. (k -> Type) -> Type
type GqlServer f = GqlServerM (GqlIo f)

test :: GqlServer Effect
test = start { root: { query: { hello: "world" }, mutation: {} } }