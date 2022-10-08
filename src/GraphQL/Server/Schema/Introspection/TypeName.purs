module GraphQL.Server.Schema.Introspection.TypeName where


import Data.Argonaut (Json)
import Data.Generic.Rep (class Generic, Constructor)
import GraphQL.Resolver.Resolver.GqlObject (GqlObj)

class GqlTypeName :: Type -> Symbol -> Constraint
class GqlTypeName a name | a -> name

instance GqlTypeName (GqlObj name a) name
else instance GqlTypeName Boolean "Boolean"
else instance GqlTypeName Int "Int"
else instance GqlTypeName Number "Float"
else instance GqlTypeName String "String"
else instance GqlTypeName Json "Json"
else instance Generic a (Constructor name t) => GqlTypeName a name

