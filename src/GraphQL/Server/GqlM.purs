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

newtype GqlM a = GqlM (ReaderT GqlEnv Aff a)
newtype GqlParM a = GqlParM (ReaderT GqlEnv ParAff a)

type GqlEnv =
  { depth :: Int
  , index :: Maybe Int
  , path :: Path
  , request :: Request
  , variables :: Object Json
  }

-- | Similar to `pure` but wraps the value in a `GqlM` monad.
-- | Helps with type inference in resolvers.
gPure :: forall a. a -> GqlM a
gPure = pure

runGqlM :: forall a. Request -> Object Json -> GqlM a -> Aff a
runGqlM request variables (GqlM a) = runReaderT a
  { depth: 0
  , index: Nothing
  , path: Nil
  , request
  , variables
  }

derive instance Newtype (GqlM a) _
derive newtype instance Functor GqlM
derive newtype instance Apply GqlM
derive newtype instance Applicative GqlM
derive newtype instance Bind GqlM
derive newtype instance Monad GqlM
derive newtype instance MonadEffect GqlM
derive newtype instance MonadAff GqlM
derive newtype instance MonadAsk GqlEnv GqlM
derive newtype instance MonadReader GqlEnv GqlM
derive newtype instance MonadThrow Error GqlM
derive newtype instance MonadError Error GqlM

derive instance Newtype (GqlParM a) _
derive newtype instance Functor GqlParM
derive newtype instance Apply GqlParM
derive newtype instance Applicative GqlParM

instance Parallel GqlParM GqlM where
  parallel = unwrap >>> parallel >>> wrap
  sequential = unwrap >>> sequential >>> wrap
