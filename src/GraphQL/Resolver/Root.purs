module GraphQL.Resolver.Root (GqlRoot(..), QueryRoot(..), MutationRoot(..)) where

import Data.Generic.Rep (class Generic, Argument, Constructor)
import Data.Newtype (class Newtype)
import GraphQL.Server.Gql (class Gql, class GqlObject, object)

newtype GqlRoot q m = GqlRoot { query :: q, mutation :: m }

derive instance Newtype (GqlRoot name a) _

derive instance Generic (GqlRoot q a) _

instance
  ( GqlObject (GqlRoot q m) 
  ) =>
  Gql (GqlRoot q m) where
  gql = object

newtype QueryRoot a = QueryRoot a

derive instance Newtype (QueryRoot a) _

derive instance Generic (QueryRoot a) _

instance
  ( GqlObject (QueryRoot { | a }) 
  ) =>
  Gql (QueryRoot { | a }) where
  gql = object

newtype MutationRoot a = MutationRoot a

derive instance Newtype (MutationRoot a) _

derive instance Generic (MutationRoot a) _

instance
  ( GqlObject (MutationRoot { | a })
  ) =>
  Gql (MutationRoot { | a }) where
  gql = object

