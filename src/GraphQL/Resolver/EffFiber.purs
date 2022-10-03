module GraphQL.Resolver.EffFiber where

import Prelude

import Data.Newtype (class Newtype, unwrap)
import Effect (Effect)
import Effect.Aff (Aff, Fiber, joinFiber)
import Effect.Class (liftEffect)

newtype EffFiber a = EffFiber (Effect (Fiber a))

derive instance Newtype (EffFiber a) _

derive instance Functor EffFiber

instance Apply EffFiber where 
  apply (EffFiber f) (EffFiber a) = EffFiber (apply (map apply f) a)

instance Applicative EffFiber where 
  pure = EffFiber <<< pure <<< pure


toAff :: EffFiber ~> Aff
toAff = unwrap >>> liftEffect >=> joinFiber