module GraphQL.Resolver.InstanceCache where

import Effect.Aff (Aff, Error)
import GraphQL.Resolver.JsonResolver (Resolver)
import HTTPure (Request)


data Nil = Nil

data Cons :: forall k. Symbol -> Type -> k -> Type
data Cons sym ty rest = Cons (Request -> ty ->  Resolver Error Aff)