module GraphQL.Resolver.EvalGql where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import GraphQL.Resolver.GqlIo (GqlIo(..))
import HTTPure (Request)

class EvalGql m where
  evalGql :: Request -> m ~> Aff

instance EvalGql m => EvalGql (GqlIo m) where
  evalGql r (GqlIo a) = evalGql r a

instance EvalGql Effect where
  evalGql _ a = liftEffect a

instance EvalGql Aff where
  evalGql _ a = identity a
