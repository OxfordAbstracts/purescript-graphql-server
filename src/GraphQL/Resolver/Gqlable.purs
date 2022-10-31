module GraphQL.Resolver.Gqlable where

import Prelude

import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import GraphQL.Resolver.GqlIo (GqlIo)
import HTTPure (Request)

class Gqlable m where
  toAff :: Request -> m ~> Aff

instance Gqlable (GqlIo Aff) where
  toAff _ = unwrap

instance Gqlable (GqlIo Effect) where
  toAff _ = unwrap >>> liftEffect

instance Gqlable Aff where
  toAff _ = identity
