module GraphQL.Resolver.EvalGql where

import Prelude

import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import GraphQL.Resolver.GqlIo (GqlIo)
import HTTPure (Request)

class EvalGql m where
  evalGql :: Request -> m ~> Aff

instance EvalGql (GqlIo Aff) where
  evalGql _ = unwrap

instance EvalGql (GqlIo Effect) where
  evalGql _ = unwrap >>> liftEffect

instance EvalGql Aff where
  evalGql _ = identity
