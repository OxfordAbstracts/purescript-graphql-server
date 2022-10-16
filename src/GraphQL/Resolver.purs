module GraphQL.Resolver (rootResolver) where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either(..), note)
import Data.Maybe (Maybe)
import GraphQL.Resolver.JsonResolver (Resolver)
import GraphQL.Resolver.Merge (mergeResolvers)
import GraphQL.Resolver.Root (GqlRoot(..), QueryRoot)
import GraphQL.Resolver.ToResolver (class ToResolver, toResolver)
import GraphQL.Server.Schema (class GetSchema, getSchema)
import GraphQL.Server.Schema.Introspection (makeIntrospectionResolver)


-- | Create a root resolver from a root record
rootResolver
  :: forall query mutation m
   . Applicative m
  => ToResolver (GqlRoot (QueryRoot {|query})  mutation) m
  => GetSchema (GqlRoot (QueryRoot {|query}) mutation)
  => { query :: {|query}, mutation :: mutation }
  -> Either String (Resolver m)
rootResolver root = do
  let
    introspectionResolver  = makeIntrospectionResolver schema
    dataResolver = toResolver root'
  note "Root or schema root is not of `Fields` contructor"
    $ mergeResolvers dataResolver introspectionResolver

  where
  root' = GqlRoot root
  schema = getSchema root'



-- class RootResolverTypes a where
--   rootResolverTypes
--     :: a
--     -> { query :: IType
--        , mutation :: Maybe IType
--        }


-- instance RootResolverTypes (GqlRoot query Unit) where
--   rootResolverTypes _ = 
--   { query: queryType, mutation: Just mutationType }
--     where
--     queryType = IType { name: "Query", description: Nothing, kind: IKObject }
--     mutationType = IType { name: "Mutation", description: Nothing, kind: IKObject }


