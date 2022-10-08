module GraphQL.Server.Schema.ToGqlType where

import Prelude

import Data.GraphQL.AST as AST
import Data.List (List)
import Data.Maybe (Maybe)
import Data.Symbol (class IsSymbol, reflectSymbol)
import GraphQL.Server.Schema.Introspection.TypeName (class GqlTypeName)
import Type.Proxy (Proxy(..))

class ToGqlType :: forall k. k -> Constraint
class ToGqlType a where
  toGqlType :: Proxy a -> AST.Type
  
instance (ToGqlType b) => ToGqlType (a -> b) where
  toGqlType _ = toGqlType (Proxy :: Proxy b)

else instance ToGqlType a => ToGqlType (Maybe (Array a)) where
  toGqlType _ = AST.Type_ListType $ AST.ListType $ toGqlType (Proxy :: Proxy a)
else instance ToGqlType a => ToGqlType (Maybe (List a)) where
  toGqlType _ = AST.Type_ListType $ AST.ListType $ toGqlType (Proxy :: Proxy a)

else instance ToGqlType a => ToGqlType (Array a) where
  toGqlType _ = AST.Type_NonNullType $ AST.NonNullType_ListType $ AST.ListType $ toGqlType (Proxy :: Proxy a)
else instance ToGqlType a => ToGqlType (Maybe (List a)) where
  toGqlType _ = AST.Type_NonNullType $ AST.NonNullType_ListType $ AST.ListType $ toGqlType (Proxy :: Proxy a)

else instance (GqlTypeName a name, IsSymbol name) => ToGqlType (Maybe a) where
  toGqlType _ = AST.Type_NamedType $ AST.NamedType $ reflectSymbol (Proxy :: Proxy name)
else instance (GqlTypeName a name, IsSymbol name) => ToGqlType a where
  toGqlType _ = AST.Type_NonNullType $ AST.NonNullType_NamedType $ AST.NamedType $ reflectSymbol (Proxy :: Proxy name)
