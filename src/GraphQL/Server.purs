module GraphQL.Server
  ( GqlServerM(..)
  , GqlServer
  , start
  , defaultOpts
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Control.Parallel (class Parallel)
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import GraphQL.Resolver (RootResolver, rootResolver)
import GraphQL.Resolver.Error (class CustomResolverError)
import GraphQL.Resolver.GqlIo (GqlIo)
import GraphQL.Resolver.EvalGql (class EvalGql)
import GraphQL.Resolver.Root (GqlRoot, MutationRoot, QueryRoot)
import GraphQL.Resolver.ToResolver (class ToResolver)
import GraphQL.Server.GqlResM (toResponse)
import GraphQL.Server.HandleRequest (handleRequest)
import GraphQL.Server.Schema (class GetSchema)
import GraphQL.Server.Schema.Introspection (IntrospectionRow)
import HTTPure (ServerM, Request, serve)
import Prim.Row (class Nub)

-- | Boot up the server
start
  :: forall query mutation withIntrospection m f err
   . EvalGql m
  => MonadError err m
  => CustomResolverError err
  => Parallel f m
  => Nub (IntrospectionRow query) withIntrospection
  => ToResolver err ((QueryRoot { | withIntrospection })) m
  => ToResolver err (MutationRoot mutation) m
  => GetSchema (GqlRoot (QueryRoot { | query }) (MutationRoot mutation))
  => ServerOpts
  -> { query :: { | query }, mutation :: mutation }
  -> GqlServerM m
start
  { isAuthorized
  }
  root =
  GqlServerM $ serve port handler onStart
  where
  handler = handleRequest isAuthorized resolvers >>> toResponse

  resolvers :: RootResolver err m
  resolvers = rootResolver root

  port = 9000
  onStart = do
    log $ "Graphql server listening at http://0.0.0.0:" <> show port

type ServerOpts =
  { isAuthorized :: Request -> Aff Boolean
  , allowIntrospection :: Request -> Aff Boolean
  }

defaultOpts :: ServerOpts
defaultOpts =
  { isAuthorized: const $ pure true
  , allowIntrospection: const $ pure true
  }

newtype GqlServerM :: forall k. k -> Type
newtype GqlServerM f = GqlServerM ServerM

type GqlServer :: forall k. (k -> Type) -> Type
type GqlServer f = GqlServerM (GqlIo f)