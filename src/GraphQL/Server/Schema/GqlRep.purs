module GraphQL.Server.GqlRep where

import Data.Argonaut (Json, JsonDecodeError)
import Data.Either (Either)

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

class Scalar ::  Type -> Symbol -> Constraint
class
  Scalar a name
  | a -> name
  where 
  encodeScalar :: a -> Json
  decodeScalar :: Json -> Either JsonDecodeError a
  
