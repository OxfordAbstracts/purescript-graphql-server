module GraphQL.Server.GqlRep where


-- | How a Purescript type is represented in GraphQL
-- | This then adds type safety for its serialization and introspection type
class GqlRep :: Type -> Type -> Symbol -> Constraint
class
  GqlRep a gqlType name
  | a -> name
  , a -> gqlType
  , name -> gqlType
  , gqlType -> name


data GObject

data GEnum

data GUnion
