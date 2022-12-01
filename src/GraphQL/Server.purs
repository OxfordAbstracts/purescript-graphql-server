module GraphQL.Server
  ( defaultOpts
  , start
  ) where

import Prelude

import Data.Newtype (class Newtype)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect, liftEffect)
import GraphQL.Resolver (RootResolver, rootResolver)
import GraphQL.Server.Gql (class Gql)
import GraphQL.Server.GqlM (GqlM)
import GraphQL.Server.GqlResM (toResponse)
import GraphQL.Server.HandleRequest (handleRequest)
import GraphQL.Server.Introspection (IntrospectionRow)
import GraphQL.Server.Resolver.Root (GqlRoot, MutationRoot, QueryRoot)
import GraphQL.Server.Schema (class GetSchema)
import HTTPure (ServerM, Request, serve)
import Prim.Row (class Nub, class Union)
import Safe.Coerce (coerce)
import Unsafe.Coerce (unsafeCoerce)

-- | Create and start a server. This is the main entry point for graphql-server. 
-- | Takes a ServerOptions with config arguments and a resolver root. 
-- | Returns an `GqlServerM` containing the server's effects.
start
  :: forall query mutation withIntrospection env
   . Union query (IntrospectionRow ()) (IntrospectionRow query)
  => Nub (IntrospectionRow query) withIntrospection
  => Gql env ((QueryRoot { | query }))
  => Gql env ((QueryRoot { | withIntrospection }))
  => Gql env (MutationRoot mutation)
  => GetSchema env (GqlRoot (QueryRoot { | query }) (MutationRoot mutation))
  => ServerOptions env
  -> { query :: { | query }, mutation :: mutation }
  -> ServerM
start
  { isAuthorized
  , mkEnv
  , onStart
  , port
  }
  root = serve port handler onStart
  where 
  handler = handleRequest isAuthorized mkEnv resolvers >>> toResponse

  resolvers :: GqlM env (RootResolver env)
  resolvers = rootResolver root


type ServerOptions env =
  -- | Run an effect on server start. 
  { onStart :: Effect Unit
  -- | Which port to listen on. 
  , port :: Int
  -- | Whether a request is authorized at the top level. 
  , isAuthorized :: Request -> Aff Boolean
  -- | Whether a request is allowed to introspect the schema. 
  , allowIntrospection :: Request -> Aff Boolean
  , mkEnv :: Request -> Aff env
  }

defaultOpts :: ServerOptions Unit
defaultOpts =
  { onStart: pure unit
  , port: 9000
  , isAuthorized: const $ pure true
  , allowIntrospection: const $ pure true
  , mkEnv: const $ pure unit
  }

-- | A newtype around HTTPure's `ServerM` with a phantom type to help with type inference
-- newtype GqlServerM :: forall k. k -> Type
-- newtype GqlServerM f = GqlServerM ServerM

-- derive instance Newtype (GqlServerM f) _
-- derive newtype instance MonadEffect (GqlServerM f)

-- type GqlServer = GqlServerM GqlM

-- -- | Run a `GqlServerM` as an `Effect`.
-- asEffect :: forall m. GqlServerM m -> Effect CloseServer
-- asEffect = coerce

-- -- | Lift a `GqlServerM` into any `MonadEffect` monad
-- liftServer :: forall p m. MonadEffect m => GqlServerM p -> m CloseServer
-- liftServer = asEffect >>> liftEffect

type CloseServer = Effect Unit -> Effect Unit