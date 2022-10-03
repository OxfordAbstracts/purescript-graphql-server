module GraphQL.Resolver.Resolver.GqlObject where

import Prelude

import Data.Newtype (class Newtype)

newtype GqlObj :: Symbol -> Type -> Type
newtype GqlObj name rec = GqlObj rec

derive instance Newtype (GqlObj name a) _

newtype GqlNew a = GqlNew a

derive instance Newtype (GqlNew a) _

derive instance Functor (GqlObj name)