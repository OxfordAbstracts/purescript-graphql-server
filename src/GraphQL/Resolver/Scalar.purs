module GraphQL.Resolver.Scalar where

-- Scalars

newtype Scalar :: forall k. k -> Type -> Type
newtype Scalar name rec = Scalar rec

type GqlInt = Scalar "Int" Int

gqlInt :: Int -> GqlInt
gqlInt = Scalar

type GqlNumber = Scalar "Float" Number

gqlNumber :: Number -> GqlNumber
gqlNumber = Scalar

type GqlString = Scalar "String" String

gqlString :: String -> GqlString
gqlString = Scalar

type GqlBoolean = Scalar "Boolean" Boolean

gqlBoolean :: Boolean -> GqlBoolean
gqlBoolean = Scalar
