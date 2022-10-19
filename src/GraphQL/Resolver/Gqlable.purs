module GraphQL.Resolver.Gqlable where

import Prelude

import Data.Newtype (unwrap, wrap)
import Effect (Effect)
import Effect.Aff (Aff, ParAff, joinFiber, launchSuspendedAff, parallel, sequential)
import Effect.Class (liftEffect)
import GraphQL.Resolver.EffFiber (EffFiber)
import GraphQL.Resolver.GqlIo (GqlIo, hoistGql)

class (Applicative a, Monad m) <= Gqlable a m | a -> m where
  gqlSequential :: a ~> m
  gqlParallel :: m ~> a
  toAff :: a ~> Aff

instance Gqlable (GqlIo Effect) (GqlIo Effect) where
  gqlSequential = identity
  gqlParallel = identity
  toAff = unwrap >>> liftEffect

instance Gqlable (GqlIo Aff) (GqlIo Aff) where
  gqlSequential = identity
  gqlParallel = identity
  toAff = unwrap

instance Gqlable Aff Aff where
  gqlSequential = identity
  gqlParallel = identity
  toAff = identity

instance Gqlable (GqlIo ParAff) (GqlIo Aff) where
  gqlSequential = sequential
  gqlParallel = parallel
  toAff = unwrap >>> sequential

instance Gqlable (GqlIo EffFiber) (GqlIo Aff) where
  gqlSequential = hoistGql effFiberToAff
  gqlParallel = hoistGql (launchSuspendedAff >>> wrap)
  toAff = unwrap >>> effFiberToAff

effFiberToAff :: EffFiber ~> Aff
effFiberToAff = unwrap >>> liftEffect >=> joinFiber
