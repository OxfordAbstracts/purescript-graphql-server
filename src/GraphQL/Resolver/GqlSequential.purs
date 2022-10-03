module GraphQL.Resolver.GqlSequential where

import Prelude

import Data.Newtype (wrap)
import Effect (Effect)
import Effect.Aff (Aff, ParAff, launchSuspendedAff, parallel, sequential)
import GraphQL.Resolver.EffFiber (EffFiber, toAff)
import GraphQL.Resolver.GqlIo (GqlIo, hoistGql)

class (Applicative a, Monad m) <= GqlSequential a m | a -> m where
  gqlSequential :: a ~> m
  gqlParallel :: m ~> a

instance GqlSequential (GqlIo Effect) (GqlIo Effect) where
  gqlSequential = identity
  gqlParallel = identity

instance GqlSequential (GqlIo Aff) (GqlIo Aff) where
  gqlSequential = identity
  gqlParallel = identity

instance GqlSequential Aff Aff where
  gqlSequential = identity
  gqlParallel = identity

instance GqlSequential (GqlIo ParAff) (GqlIo Aff) where
  gqlSequential = sequential
  gqlParallel = parallel

instance GqlSequential (GqlIo EffFiber) (GqlIo Aff) where
  gqlSequential = hoistGql toAff
  gqlParallel = hoistGql (launchSuspendedAff >>> wrap)

