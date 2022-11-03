module GraphQL.Resolver.Hoist where

import Prelude

import GraphQL.Resolver.JsonResolver (Resolver(..))

hoistResolver :: forall m n err. Functor m => (m ~> n) -> Resolver err m -> Resolver err n
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

mapError :: forall err err' m. Functor m => (err -> err') -> Resolver err m -> Resolver err' m
mapError fn = case _ of
  Node a -> Node a
  ListResolver a -> ListResolver $ map (mapError fn) a
  Fields { typename, fields } -> Fields
    { typename
    , fields: fields <#> \{ name, resolver } ->
        { name
        , resolver: map (mapError fn) resolver
        }
    }
  ResolveAsync a -> ResolveAsync (map (mapError fn) a)
  NullableResolver a -> NullableResolver $ map (mapError fn) a
  FailedResolver err -> FailedResolver (map fn err)