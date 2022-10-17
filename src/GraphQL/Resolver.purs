module GraphQL.Resolver (rootResolver) where

import Prelude

import GraphQL.Resolver.JsonResolver (Resolver)
import GraphQL.Resolver.Root (GqlRoot(..), MutationRoot(..), QueryRoot(..))
import GraphQL.Resolver.ToResolver (class ToResolver, toResolver)
import GraphQL.Server.Schema (class GetSchema, getSchema)
import GraphQL.Server.Schema.Introspection (IntrospectionRow, getIntrospection)
import Prim.Row (class Nub)
import Record as Record

-- | Create a root resolver from a root record
rootResolver
  :: forall query mutation m withIntrospection
   . Applicative m
  => Nub (IntrospectionRow query) withIntrospection
  => ToResolver (GqlRoot (QueryRoot { | withIntrospection }) (MutationRoot mutation)) m
  => GetSchema (GqlRoot (QueryRoot { | query }) (MutationRoot mutation))
  => { query :: { | query }, mutation :: mutation }
  -> (Resolver m)
rootResolver root =
  toResolver $ GqlRoot root
    { query = QueryRoot (Record.merge introspection query :: { | withIntrospection })
    , mutation = MutationRoot root.mutation
    }
  where
  root'@(GqlRoot { query: QueryRoot query }) = GqlRoot root
    { query = QueryRoot root.query
    , mutation = MutationRoot root.mutation
    }
  schema = getSchema root'

  introspection = getIntrospection schema

test0 :: forall m. Applicative m => Resolver m
test0 = rootResolver { query: { foo: "bar" }, mutation: unit }

test1 :: forall m. Applicative m => Resolver m
test1 = rootResolver { query: { foo: "bar" }, mutation: { x: 1 } }