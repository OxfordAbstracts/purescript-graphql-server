module GraphQL.GqlRep where

-- | How a Purescript type is represented in GraphQL
-- | This then adds type safety for its serialization and introspection type
class GqlRep :: Type -> Type -> Symbol -> Constraint
class

  GqlRep a gqlType name
  | a -> name
  , a -> gqlType
  , name -> gqlType
  , gqlType -> name

instance GqlRep Boolean GScalar "Boolean"

instance GqlRep Int GScalar "Int"

instance GqlRep Number GScalar "Float"

instance GqlRep String GScalar "String"

data GEnum

data GScalar

data GObject

data GUnion