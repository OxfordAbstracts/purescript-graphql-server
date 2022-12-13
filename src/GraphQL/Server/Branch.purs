module GraphQL.Server.Branch where

import Prelude

-- import GraphQL.Server.Gql (class Gql, GqlProps(..), gql)
import GraphQL.Server.GqlM (GqlM)
import GraphQL.Server.Resolver.JsonResolver (Resolver(..))
import Type.Proxy (Proxy(..))

data Branch pred a b =
  Branch pred a b

class GqlIf :: forall k. k -> Type -> Constraint
class GqlIf pred env | pred -> env where
  gqlIf :: Proxy pred -> GqlM env Boolean

instance GqlIf pred env => GqlIf (Proxy pred) env where
  gqlIf _ = gqlIf (Proxy :: Proxy pred)


