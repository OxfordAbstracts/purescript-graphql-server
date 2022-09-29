module GraphQL.Resolver.GqlIo where

import Prelude

import Control.Alt (class Alt)
import Control.Lazy (class Lazy)
import Data.Functor.Invariant (class Invariant, imapF)
import Data.Newtype (class Newtype)
import Effect (Effect)
import Effect.Aff (Aff, Fiber, ParAff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)

newtype GqlIo :: forall k. (k -> Type) -> k -> Type
newtype GqlIo m a = GqlIo (m a)

type GqlAff = GqlIo Aff

type GqlParAff = GqlIo ParAff

type GqlFiber = GqlIo Fiber

type GqlEffect = GqlIo Effect

derive instance Newtype (GqlIo m a) _

derive newtype instance Eq (m a) => Eq (GqlIo m a)

derive newtype instance Ord (m a) => Ord (GqlIo m a)

derive newtype instance Bounded (m a) => Bounded (GqlIo m a)

derive newtype instance HeytingAlgebra (m a) => HeytingAlgebra (GqlIo m a)

derive newtype instance BooleanAlgebra (m a) => BooleanAlgebra (GqlIo m a)

derive newtype instance Semigroup (m a) => Semigroup (GqlIo m a)

derive newtype instance Monoid (m a) => Monoid (GqlIo m a)

derive newtype instance Semiring (m a) => Semiring (GqlIo m a)

derive newtype instance EuclideanRing (m a) => EuclideanRing (GqlIo m a)

derive newtype instance Ring (m a) => Ring (GqlIo m a)

derive newtype instance CommutativeRing (m a) => CommutativeRing (GqlIo m a)

derive newtype instance Lazy (m a) => Lazy (GqlIo m a)

instance Show (m a) => Show (GqlIo m a) where
  show (GqlIo x) = "(GqlIo m " <> show x <> ")"

derive instance Functor m => Functor (GqlIo m)

instance Functor m => Invariant (GqlIo m) where
  imap = imapF

instance Functor m => Alt (GqlIo m) where
  alt x _ = x

derive newtype instance Apply m => Apply (GqlIo m)

derive newtype instance Applicative m => Applicative (GqlIo m)

derive newtype instance Bind m => Bind (GqlIo m)

derive newtype instance Monad m => Monad (GqlIo m)

derive newtype instance MonadEffect m => MonadEffect (GqlIo m)

derive newtype instance MonadAff m => MonadAff (GqlIo m)
