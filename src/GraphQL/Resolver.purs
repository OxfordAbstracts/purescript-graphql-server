module GraphQL.Resolver (RootResolver, rootResolver) where

import Prelude

import GraphQL.Resolver.JsonResolver (Resolver)
import GraphQL.Resolver.Root (GqlRoot(..), MutationRoot(..), QueryRoot(..))
import GraphQL.Resolver.ToResolver (class ToResolver, toResolver)
import GraphQL.Server.Schema (class GetSchema, getSchema)
import GraphQL.Server.Schema.Introspection (Introspection(..), IntrospectionRow, getIntrospection)
import Prim.Row (class Nub)
import Record as Record

-- | Create a root resolver from a root record
rootResolver
  :: forall query mutation m withIntrospection
   . Applicative m
  => Nub (IntrospectionRow query) withIntrospection
  => ToResolver ((QueryRoot { | withIntrospection })) m
  => ToResolver (MutationRoot mutation) m
  => GetSchema (GqlRoot (QueryRoot { | query }) (MutationRoot mutation))
  => { query :: { | query }, mutation :: mutation }
  -> RootResolver m
rootResolver root =
  { query: toResolver $ QueryRoot (Record.merge introspection query :: { | withIntrospection })
  , mutation: toResolver $ MutationRoot root.mutation
  , introspection: Introspection introspection
  }
  where
  root'@(GqlRoot { query: QueryRoot query }) = GqlRoot root
    { query = QueryRoot root.query
    , mutation = MutationRoot root.mutation
    }
  schema = getSchema root'

  introspection = getIntrospection schema

type RootResolver m =
  { query :: Resolver m
  , mutation :: Resolver m
  , introspection :: Introspection
  }

test0 :: forall m. Applicative m => RootResolver m
test0 = rootResolver { query: { foo: "bar" }, mutation: unit }

test1 :: forall m. Applicative m => RootResolver m
test1 = rootResolver { query: { foo: "bar" }, mutation: { x: 1 } }