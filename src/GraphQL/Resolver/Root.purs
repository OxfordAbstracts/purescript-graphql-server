module GraphQL.Resolver.Root where

import Data.Newtype (class Newtype)

newtype GqlRoot q m = GqlRoot { query :: q, mutation :: m }

derive instance Newtype (GqlRoot name a) _

