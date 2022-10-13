module GraphQL.Resolver.Hoist where

import Prelude

import GraphQL.Resolver.JsonResolver (Resolver(..))

hoistResolver :: forall m n. Functor m => (m ~> n) -> Resolver m -> Resolver n
hoistResolver fn = case _ of
  Node a -> Node $ fn a
  ListResolver a -> ListResolver $ map (hoistResolver fn) a
  Fields { typename, fields } -> Fields
    { typename
    , fields: fields <#> \{ name, resolver } ->
        { name
        , resolver: map (hoistResolver fn) resolver
        }
    }
  ResolveAsync a -> ResolveAsync $ fn $ map (hoistResolver fn) a
  NullableResolver a -> NullableResolver $ map (hoistResolver fn) a
  FailedResolver err -> FailedResolver err