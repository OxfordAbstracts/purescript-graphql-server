module GraphQL.Resolver.EvalGql where

import Prelude

import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import GraphQL.Resolver.GqlIo (GqlIo(..))
import HTTPure (Request)

class EvalGql m where
  evalGql :: Request -> m ~> Aff

instance EvalGql Aff where
  evalGql _ = identity

instance EvalGql Effect where
  evalGql _ =  liftEffect

instance EvalGql m => EvalGql (GqlIo m) where
  evalGql req (GqlIo m) = evalGql req m
