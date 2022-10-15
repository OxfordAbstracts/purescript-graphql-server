module GraphQL.Resolver (rootResolver) where

import Prelude

import Data.Argonaut (Json)
import Data.Bifunctor (lmap)
import Data.Either (Either, note)
import GraphQL.Resolver.JsonResolver (Resolver)
import GraphQL.Resolver.Merge (mergeResolvers)
import GraphQL.Resolver.Root (GqlRoot(..))
import GraphQL.Resolver.ToResolver (class ToResolver, toResolver)
import GraphQL.Server.GqlError (GqlError)
import GraphQL.Server.Schema.GetDocument (class GetDocument)
import GraphQL.Server.Schema.Introspection (makeIntrospectionResolver)
import GraphQL.Server.Schema.Introspection.Schema (introspectSchema)

-- resolveRoot :: forall m. Applicative m => Resolver m -> String -> Either GqlError Json
-- resolveRoot = 

-- | Create a root resolver from a root record
rootResolver
  :: forall query mutation m
   . Applicative m
  => ToResolver (GqlRoot query mutation) m
  => GetDocument (GqlRoot query mutation)
  => { query :: query, mutation :: mutation }
  -> Either String (Resolver m)
rootResolver root = do
  introspectionSchema <- lmap show $ introspectSchema root'
  let
    introspectionResolver = makeIntrospectionResolver introspectionSchema
    dataResolver = toResolver root'
  note "Root or schema root is not of `Fields` contructor"
    $ mergeResolvers dataResolver introspectionResolver

  where
  root' = GqlRoot root

