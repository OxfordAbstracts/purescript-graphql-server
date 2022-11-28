module GraphQL.Server
  ( GqlServer
  , GqlServerM(..)
  , asEffect
  , defaultOpts
  , liftServer
  , start
  ) where

import Prelude

import Data.Newtype (class Newtype)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect, liftEffect)
import GraphQL.Resolver (RootResolver, rootResolver)
import GraphQL.Server.GqlM (GqlM)
import GraphQL.Server.Resolver.Root (GqlRoot, MutationRoot, QueryRoot)
import GraphQL.Server.Gql (class Gql)
import GraphQL.Server.GqlResM (toResponse)
import GraphQL.Server.HandleRequest (handleRequest)
import GraphQL.Server.Schema (class GetSchema)
import GraphQL.Server.Introspection (IntrospectionRow)
import HTTPure (ServerM, Request, serve)
import Prim.Row (class Nub, class Union)
import Safe.Coerce (coerce)

-- | Create and start a server. This is the main entry point for graphql-server. 
-- | Takes a ServerOptions with config arguments and a resolver root. 
-- | Returns an `GqlServerM` containing the server's effects.
start
  :: forall query mutation withIntrospection
   . Union query (IntrospectionRow ()) (IntrospectionRow query)
  => Nub (IntrospectionRow query) withIntrospection
  => Gql ((QueryRoot { | query }))
  => Gql ((QueryRoot { | withIntrospection }))
  => Gql (MutationRoot mutation)
  => GetSchema (GqlRoot (QueryRoot { | query }) (MutationRoot mutation))
  => ServerOptions
  -> { query :: { | query }, mutation :: mutation }
  -> GqlServerM Aff
start
  { isAuthorized
  , onStart
  , port
  }
  root =
  GqlServerM $ serve port handler onStart
  where
  handler = handleRequest isAuthorized resolvers >>> toResponse

  resolvers :: RootResolver
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

type GqlServer = GqlServerM GqlM

-- | Run a `GqlServerM` as an `Effect`.
asEffect :: forall m. GqlServerM m -> Effect CloseServer
asEffect = coerce

-- | Lift a `GqlServerM` into any `MonadEffect` monad
liftServer :: forall p m. MonadEffect m => GqlServerM p -> m CloseServer
liftServer = asEffect >>> liftEffect

type CloseServer = Effect Unit -> Effect Unit