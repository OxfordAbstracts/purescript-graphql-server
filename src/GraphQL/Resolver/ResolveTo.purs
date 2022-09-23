module GraphQL.Resolver.Resolver.GqlObject where

newtype GqlObj :: forall k. k -> Type -> Type
newtype GqlObj name rec = GqlObj rec
