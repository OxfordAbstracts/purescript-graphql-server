module GraphQL.Resolver.ToSchema where

import Prelude

import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..))
import GraphQL.Resolver.JsonResolver (Fields, Resolver(..), TopLevelJsonResolver)
import GraphQL.Server.Schema.Introspection.Types (IField(..), ISchema(..), IType(..))
import Unsafe.Coerce (unsafeCoerce)

-- getIntrospectionSchema
--   :: forall m
--    . Applicative m
--   => TopLevelJsonResolver m
--   -> ISchema
-- getIntrospectionSchema { query, mutation, subscription } = ISchema
--   { types: getResolverITypes query
--   , queryType: unsafeCoerce unit -- IType
--   , mutationType: unsafeCoerce unit -- Maybe IType
--   , subscriptionType: unsafeCoerce unit -- Maybe IType
--   , directives: unsafeCoerce unit -- List IDirective
--   }

-- getResolverITypes
--   :: forall m
--    . Applicative m
--   => Resolver m
--   -> List IType
-- getResolverITypes = case _ of
--   Fields { typename, fields } -> pure $ IType
--     { name: Just typename
--     , description: Nothing
--     , kind: unsafeCoerce unit
--     , fields: \_ -> Just $ List.fromFoldable   fields <#> ?D --  <#> \f -> IField $ ?D f
--     , inputFields: Nothing
--     , interfaces: Nothing
--     , enumValues: \_ ->  Nothing
--     , possibleTypes: Nothing
--     , ofType: Nothing
--     }
--   _ -> Nil
