module GraphQL.Resolver.Root (GqlRoot(..), QueryRoot(..), MutationRoot(..)) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Symbol (class IsSymbol, reflectSymbol)
import GraphQL.GqlRep (class GqlRep, GObject)
import GraphQL.Resolver.JsonResolver (Resolver(..))
import GraphQL.Resolver.ToResolver (class ToResolver, FieldMap, ToResolverProps, makeFields, toObjectResolver)
import GraphQL.Server.GqlError (ResolverError(..))
import GraphQL.Server.Schema.Introspection.GetType (class GetIFields, class GetGqlType, getObjectType)
import Heterogeneous.Folding (class HFoldlWithIndex)
import Type.Proxy (Proxy(..))

newtype GqlRoot q m = GqlRoot { query :: q, mutation :: m }

derive instance Newtype (GqlRoot name a) _

instance
  ( Applicative m
  , HFoldlWithIndex (ToResolverProps m) (FieldMap m) ({ query :: q, mutation :: mut }) (FieldMap m)
  , IsSymbol "root"
  ) =>
  ToResolver (GqlRoot q mut) m where
  toResolver (GqlRoot root) = Fields
    { fields: makeFields (reflectSymbol (Proxy :: Proxy "root")) root
    , typename: "root"
    }

newtype QueryRoot a = QueryRoot a

derive instance Generic (QueryRoot a) _

instance
  ( GetIFields { | r }
  ) =>
  GetGqlType (QueryRoot { | r }) where
  getType a = getObjectType a

instance GqlRep (QueryRoot a) GObject "QueryRoot"

instance
  ( Applicative m
  , HFoldlWithIndex (ToResolverProps m) (FieldMap m) { | a } (FieldMap m)
  ) =>
  ToResolver (QueryRoot { | a }) m where
  toResolver a = toObjectResolver a

newtype MutationRoot a = MutationRoot a

derive instance Generic (MutationRoot a) _

instance GqlRep (MutationRoot a) GObject "MutationRoot"

instance
  ( Applicative m
  , HFoldlWithIndex (ToResolverProps m) (FieldMap m) { | a } (FieldMap m)
  ) =>
  ToResolver (MutationRoot { | a }) m where
  toResolver a = toObjectResolver a

instance
  ( Applicative m
  ) =>
  ToResolver (MutationRoot Unit) m where
  toResolver _ = FailedResolver NoMutationRoot

instance
  ( GetIFields { | r }
  ) =>
  GetGqlType (MutationRoot { | r }) where
  getType a = getObjectType a
