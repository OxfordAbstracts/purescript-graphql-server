module GraphQL.Server
  ( GqlServer
  , GqlServerM(..)
  , asEffect
  , defaultOpts
  , liftServer
  , start
  )
  where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Control.Parallel (class Parallel)
import Data.Newtype (class Newtype)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect, liftEffect)
import GraphQL.Resolver (RootResolver, rootResolver)
import GraphQL.Resolver.Error (class CustomResolverError)
import GraphQL.Resolver.EvalGql (class EvalGql)
import GraphQL.Resolver.GqlIo (GqlIo)
import GraphQL.Resolver.Root (GqlRoot, MutationRoot, QueryRoot)
import GraphQL.Resolver.ToResolver (class ToResolver)
import GraphQL.Server.GqlResM (toResponse)
import GraphQL.Server.HandleRequest (handleRequest)
import GraphQL.Server.Schema (class GetSchema)
import GraphQL.Server.Schema.Introspection (IntrospectionRow)
import HTTPure (ServerM, Request, serve)
import Prim.Row (class Nub)
import Safe.Coerce (coerce)

-- | Create and start a server. This is the main entry point for graphql-server. 
-- | Takes a ServerOptions with config arguments and a resolver root. 
-- | Returns an `GqlServerM` containing the server's effects.
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
  => ServerOptions
  -> { query :: { | query }, mutation :: mutation }
  -> GqlServerM m
start
  { isAuthorized
  , onStart
  , port
  }
  root =
  GqlServerM $ serve port handler onStart
  where
  handler = handleRequest isAuthorized resolvers >>> toResponse

  resolvers :: RootResolver err m
  resolvers = rootResolver root

type ServerOptions =
  -- | Run an effect on server start. 
  { onStart :: Effect Unit
  -- | Which port to listen on. 
  , port :: Int
  -- | Whether a request is authorized at the top level. 
  , isAuthorized :: Request -> Aff Boolean
  -- | Whether a request is allowed to introspect the schema. 
  , allowIntrospection :: Request -> Aff Boolean
  }

defaultOpts :: ServerOptions
defaultOpts =
  { onStart: pure unit
  , port: 9000
  , isAuthorized: const $ pure true
  , allowIntrospection: const $ pure true
  }

-- | A newtype around HTTPure's `ServerM` with a phantom type to help with type inference
newtype GqlServerM :: forall k. k -> Type
newtype GqlServerM f = GqlServerM ServerM

derive instance Newtype (GqlServerM f) _

type GqlServer :: forall k. (k -> Type) -> Type
type GqlServer f = GqlServerM (GqlIo f)


-- | Run a `GqlServerM` as an `Effect`.
asEffect :: forall m. GqlServerM m -> Effect CloseServer
asEffect = coerce


-- | Lift a `GqlServerM` into any `MonadEffect` monad
liftServer :: forall p m. MonadEffect m => GqlServerM p -> m CloseServer
liftServer = asEffect >>> liftEffect

type CloseServer = Effect Unit -> Effect Unit 