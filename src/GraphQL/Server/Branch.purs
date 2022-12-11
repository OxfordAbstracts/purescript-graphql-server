module GraphQL.Server.Branch where

import GraphQL.Server.GqlM (GqlM)
import Type.Proxy (Proxy)

data Branch :: forall k. k -> Type -> Type -> Type -> Type
data Branch pred env a b =
  Branch (Proxy pred) (GqlM env a) (GqlM env b)

class GqlIf :: forall k. k -> Type -> Constraint
class GqlIf pred env where
  gqlIf :: Proxy pred -> GqlM env Boolean
