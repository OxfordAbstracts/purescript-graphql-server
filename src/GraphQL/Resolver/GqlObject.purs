module GraphQL.Resolver.Resolver.GqlObject where

import Data.Newtype (class Newtype)

newtype GqlObj :: forall k. k -> Type -> Type
newtype GqlObj name rec = GqlObj rec

derive instance Newtype (GqlObj name a) _