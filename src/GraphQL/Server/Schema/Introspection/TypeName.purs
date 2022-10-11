module GraphQL.Server.Schema.Introspection.TypeName where

import Data.Argonaut (Json)
import Data.Generic.Rep (class Generic, Constructor)
import GraphQL.Resolver.Resolver.GqlObject (GqlObj)

class GqlTypeName :: Type -> Symbol -> Constraint
class GqlTypeName a name | a -> name

instance GqlTypeName (GqlObj name a) name
instance GqlTypeName Boolean "Boolean"
instance GqlTypeName Int "Int"
instance GqlTypeName Number "Float"
instance GqlTypeName String "String"
instance GqlTypeName Json "Json"

-- else instance Generic a (Constructor name t) => GqlTypeName a name

