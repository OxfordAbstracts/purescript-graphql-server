module GraphQL.Resolver.EvalGql where

import Prelude

import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Writer (WriterT, runWriterT)
import Data.Tuple (fst)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import GraphQL.Resolver.GqlIo (GqlIo(..))
import GraphQL.Server.Request (GqlRequest(..))
import HTTPure (Request)

class EvalGql m where
  evalGql :: Request -> m ~> Aff

instance EvalGql m => EvalGql (GqlIo m) where
  evalGql r (GqlIo a) = evalGql r a

instance EvalGql m => EvalGql (ReaderT GqlRequest m) where
  evalGql r a = evalGql r $ runReaderT a (GqlRequest r)

instance (Functor m, EvalGql m) => EvalGql (WriterT w m) where
  evalGql r a = evalGql r $ fst <$> runWriterT a 

instance EvalGql Effect where
  evalGql _ a = liftEffect a

instance EvalGql Aff where
  evalGql _ a = identity a
