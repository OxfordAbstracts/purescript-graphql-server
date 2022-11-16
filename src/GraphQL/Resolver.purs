module GraphQL.Resolver (RootResolver, rootResolver) where

import Prelude

import Effect.Aff (Aff)
import Effect.Exception (Error)
import GraphQL.Resolver.InstanceCache as IC
import GraphQL.Resolver.JsonResolver (Resolver)
import GraphQL.Resolver.Root (GqlRoot(..), MutationRoot(..), QueryRoot(..))
import GraphQL.Resolver.ToResolver (class ToResolver, toResolver)
import GraphQL.Server.Schema (class GetSchema, getSchema)
import GraphQL.Server.Schema.Introspection (Introspection(..), IntrospectionRow, getIntrospection)
import Prim.Row (class Nub)
import Record as Record

-- | Create a root resolver from a root record
rootResolver
  :: forall query mutation m withIntrospection err
   . Applicative m
  => Nub (IntrospectionRow query) withIntrospection
  => ToResolver IC.Nil ((QueryRoot { | withIntrospection }))
  => ToResolver  IC.Nil (MutationRoot mutation)
  => GetSchema (GqlRoot (QueryRoot { | query }) (MutationRoot mutation))
  => { query :: { | query }, mutation :: mutation }
  -> RootResolver Error Aff
rootResolver root =
  { query: toResolver  IC.Nil $ QueryRoot (Record.merge introspection query :: { | withIntrospection })
  , mutation: toResolver IC.Nil $ MutationRoot root.mutation
  , introspection: Introspection introspection
  }
  where
  root'@(GqlRoot { query: QueryRoot query }) = GqlRoot root
    { query = QueryRoot root.query
    , mutation = MutationRoot root.mutation
    }
  schema = getSchema root'

  introspection = getIntrospection schema

type RootResolver err m =
  { query :: Resolver err m
  , mutation :: Resolver err m
  , introspection :: Introspection
  }