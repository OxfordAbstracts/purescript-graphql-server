module GraphQL.Server.Schema.Introspection.Resolver where

import Prelude

import Effect.Aff (Aff)
import GraphQL.Resolver.GqlIo (GqlIo)
import GraphQL.Resolver.Gqlable (class Gqlable)
import GraphQL.Resolver.JsonResolver (Resolver)
import GraphQL.Resolver.Resolver.GqlObject (GqlObj(..))
import GraphQL.Resolver.ToResolver (toResolver)
import GraphQL.Server.Schema.Introspection.Types (ISchema(..))


makeIntrospectionResolver :: ISchema -> Resolver (GqlIo Aff)
makeIntrospectionResolver schema = toResolver schema