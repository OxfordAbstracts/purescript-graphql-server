module GraphQL.Server.Schema.GqlTypeName where

import GraphQL.Resolver.Resolver.GqlObject (GqlObj)

class GqlTypeName :: Type -> Symbol -> Constraint
class GqlTypeName a name | a -> name

instance GqlTypeName (GqlObj name a) name
instance GqlTypeName Boolean "Boolean"
instance GqlTypeName Int "Int"
instance GqlTypeName Number "Float"
instance GqlTypeName String "String"
instance GqlTypeName b name => GqlTypeName (a -> b) name