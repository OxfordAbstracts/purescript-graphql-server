module GraphQL.Server.GqlM where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT(..), runReaderT)
import Control.Parallel (class Parallel, parallel, sequential)
import Data.Argonaut (Json)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Effect.Aff (Aff, Error, ParAff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Foreign.Object (Object)
import GraphQL.Server.Resolver.Path (Path)
import HTTPure (Request)

newtype GqlM env a = GqlM (ReaderT (GqlEnv env) Aff a)
newtype GqlParM env a = GqlParM (ReaderT (GqlEnv env) ParAff a)

type GqlEnv env =
  { env :: env
  , depth :: Int
  , index :: Maybe Int
  , path :: Path
  , request :: Request
  , variables :: Object Json
  }

-- | Similar to `pure` but wraps the value in a `GqlM` monad.
-- | Helps with type inference in resolvers.
gPure :: forall env a. a -> GqlM env a
gPure = pure

runGqlM :: forall env a. (Request -> Aff env) -> Request -> Object Json -> GqlM env a -> Aff a
runGqlM mkEnv request variables (GqlM a) = do
  env <- mkEnv request
  runReaderT a
    { depth: 0
    , index: Nothing
    , path: Nil
    , request
    , variables
    , env
    }

derive instance Newtype (GqlM env a) _
derive newtype instance Functor (GqlM env)
derive newtype instance Apply (GqlM env)
derive newtype instance Applicative (GqlM env)
derive newtype instance Bind (GqlM env)
derive newtype instance Monad (GqlM env)
derive newtype instance MonadEffect (GqlM env)
derive newtype instance MonadAff (GqlM env)
derive newtype instance MonadAsk (GqlEnv env) (GqlM env)
derive newtype instance MonadReader (GqlEnv env) (GqlM env)
derive newtype instance MonadThrow Error (GqlM env)
derive newtype instance MonadError Error (GqlM env)

derive instance Newtype ((GqlParM env) a) _
derive newtype instance Functor (GqlParM env)
derive newtype instance Apply (GqlParM env)
derive newtype instance Applicative (GqlParM env)

instance Parallel (GqlParM env) (GqlM env) where
  parallel = unwrap >>> parallel >>> wrap
  sequential = unwrap >>> sequential >>> wrap
