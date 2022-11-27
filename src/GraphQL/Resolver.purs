module GraphQL.Resolver (RootResolver, rootResolver) where

import Prelude

import GraphQL.Resolver.JsonResolver (Resolver)
import GraphQL.Resolver.Root (GqlRoot(..), MutationRoot(..), QueryRoot(..))
import GraphQL.Server.Gql (class Gql, GqlProps(..), gql)
import GraphQL.Server.Schema (class GetSchema, getSchema)
import GraphQL.Server.Schema.Introspection (Introspection(..), IntrospectionRow, getIntrospection)
import HTTPure (Request)
import Prim.Row (class Nub, class Union)
import Record as Record

-- | Create a root resolver from a root record
rootResolver
  :: forall query mutation withIntrospection
   . Union query (IntrospectionRow ()) (IntrospectionRow query)
  => Nub (IntrospectionRow query) withIntrospection
  => Gql ((QueryRoot { | query }))
  => Gql ((QueryRoot { | withIntrospection }))
  => Gql (MutationRoot mutation)
  => GetSchema (GqlRoot (QueryRoot { | query }) (MutationRoot mutation))
  => { query :: { | query }, mutation :: mutation }
  -> RootResolver
rootResolver root =
  { query: withIntrospectionProps.resolver $ QueryRoot $ Record.merge root.query introspection
  , mutation: mutationProps.resolver $ MutationRoot root.mutation
  , introspection: Introspection introspection
  }
  where
  GqlProps mutationProps = gql unit
  GqlProps withIntrospectionProps = gql unit :: GqlProps (QueryRoot { | withIntrospection })
  
  root'= GqlRoot root
    { query = QueryRoot root.query
    , mutation = MutationRoot root.mutation
    }
    
  schema = getSchema root'

  introspection = getIntrospection schema

type RootResolver =
  { query :: Request -> Resolver
  , mutation :: Request -> Resolver
  , introspection :: Introspection
  }