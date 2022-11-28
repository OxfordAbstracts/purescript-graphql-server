module GraphQL.Resolver (RootResolver, rootResolver) where

import Prelude

import GraphQL.Server.Gql (class Gql, GqlProps(..), gql)
import GraphQL.Server.Introspection (Introspection(..), IntrospectionRow, getIntrospection)
import GraphQL.Server.Resolver.JsonResolver (Resolver)
import GraphQL.Server.Resolver.Root (GqlRoot(..), MutationRoot(..), QueryRoot(..))
import GraphQL.Server.Schema (class GetSchema, getSchema)
import HTTPure (Request)
import Prim.Row (class Nub, class Union)
import Record as Record
import Type.Proxy (Proxy(..))

-- | Create a root resolver from a root record
rootResolver
  :: forall query mutation withIntrospection env
   . Union query (IntrospectionRow ()) (IntrospectionRow query)
  => Nub (IntrospectionRow query) withIntrospection
  => Gql env ((QueryRoot { | query }))
  => Gql env ((QueryRoot { | withIntrospection }))
  => Gql env (MutationRoot mutation)
  => GetSchema env (GqlRoot (QueryRoot { | query }) (MutationRoot mutation))
  => { query :: { | query }, mutation :: mutation }
  -> RootResolver env
rootResolver root =
  { query: withIntrospectionProps.resolver $ QueryRoot $ Record.merge root.query introspection
  , mutation: mutationProps.resolver $ MutationRoot root.mutation
  , introspection: Introspection introspection
  }
  where
  GqlProps mutationProps = gql unit
  GqlProps withIntrospectionProps = gql unit :: GqlProps env (QueryRoot { | withIntrospection })

  root' = GqlRoot root
    { query = QueryRoot root.query
    , mutation = MutationRoot root.mutation
    }

  schema = getSchema (Proxy :: Proxy env) root'

  introspection = getIntrospection schema

type RootResolver  env =
  { query :: Request -> Resolver env
  , mutation :: Request -> Resolver env
  , introspection :: Introspection
  }