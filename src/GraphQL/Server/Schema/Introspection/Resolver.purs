module GraphQL.Server.Schema.Introspection.Resolver where

import Prelude

import GraphQL.Resolver.GqlIo (GqlIo)
import GraphQL.Resolver.JsonResolver (Resolver)
import GraphQL.Resolver.ToResolver (toResolver)
import GraphQL.Server.Schema.Introspection.Types (ISchema)

makeIntrospectionResolver :: forall m. Applicative m => ISchema -> Resolver (GqlIo m)
makeIntrospectionResolver schema = toResolver schema