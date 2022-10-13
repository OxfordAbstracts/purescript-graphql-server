module GraphQL.Resolver.Merge where

import Prelude

import Data.Map as Map
import Data.Maybe (Maybe(..))
import GraphQL.Resolver.JsonResolver (Fields, Resolver(..))

-- Shallow union of resolvers if they are both objects
mergeResolvers :: forall m. Resolver m -> Resolver m -> Maybe (Resolver m)
mergeResolvers = case _, _ of
  Fields f1, Fields f2 -> Just $ Fields $ fieldsUnion f1 f2
  _, _ -> Nothing

fieldsUnion :: forall m. Fields m -> Fields m -> Fields m
fieldsUnion f1 f2 = { typename: f1.typename, fields: Map.union f1.fields f2.fields }