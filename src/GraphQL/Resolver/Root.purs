module GraphQL.Resolver.Root (GqlRoot(..), QueryRoot(..), MutationRoot(..)) where

import Data.Generic.Rep (class Generic, Argument, Constructor)
import Data.Newtype (class Newtype)
import GraphQL.Server.Gql (class Gql, FieldMap, ToResolverProps, object)
import GraphQL.Server.Schema.Introspection.GetType (class GetIFields)
import Heterogeneous.Folding (class HFoldlWithIndex)

newtype GqlRoot q m = GqlRoot { query :: q, mutation :: m }

derive instance Newtype (GqlRoot name a) _


newtype QueryRoot a = QueryRoot a

derive instance Newtype (QueryRoot a) _

derive instance Generic (QueryRoot a) _

instance
  ( HFoldlWithIndex ToResolverProps FieldMap { | a } FieldMap
  , GetIFields { | a }
  ) =>
  Gql (QueryRoot { | a }) where
  gql = object

newtype MutationRoot a = MutationRoot a

derive instance Newtype (MutationRoot a) _

derive instance Generic (MutationRoot a) _

instance
  ( HFoldlWithIndex ToResolverProps FieldMap { | a } FieldMap
  , GetIFields { | a }
  ) =>
  Gql (MutationRoot { | a }) where
  gql = object

