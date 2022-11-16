module GraphQL.Resolver.InstanceCache where

import Effect.Aff (Aff, Error)
import GraphQL.Resolver.JsonResolver (Resolver)
import HTTPure (Request)

data Nil = Nil

data Cons :: Type -> Type -> Type -> Type
data Cons ty rep rest = Cons (Request -> ty -> rep -> Resolver Error Aff) rest