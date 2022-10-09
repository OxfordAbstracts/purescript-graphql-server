module GraphQL.Server.Schema.Introspection.TypeDefinitionGetter where

import Prelude

import Data.GraphQL.AST as AST
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import GraphQL.Server.Schema.Introspection.Types as ITypes

getTypeDefName :: AST.TypeDefinition -> String
getTypeDefName = case _ of
  AST.TypeDefinition_ScalarTypeDefinition (AST.ScalarTypeDefinition t) -> t.name
  AST.TypeDefinition_ObjectTypeDefinition (AST.ObjectTypeDefinition t) -> t.name
  AST.TypeDefinition_InterfaceTypeDefinition (AST.InterfaceTypeDefinition t) -> t.name
  AST.TypeDefinition_UnionTypeDefinition (AST.UnionTypeDefinition t) -> t.name
  AST.TypeDefinition_EnumTypeDefinition (AST.EnumTypeDefinition t) -> t.name
  AST.TypeDefinition_InputObjectTypeDefinition (AST.InputObjectTypeDefinition t) -> t.name

getTypeDefDescription :: AST.TypeDefinition -> Maybe String
getTypeDefDescription = case _ of
  AST.TypeDefinition_ScalarTypeDefinition (AST.ScalarTypeDefinition t) -> t.description
  AST.TypeDefinition_ObjectTypeDefinition (AST.ObjectTypeDefinition t) -> t.description
  AST.TypeDefinition_InterfaceTypeDefinition (AST.InterfaceTypeDefinition t) -> t.description
  AST.TypeDefinition_UnionTypeDefinition (AST.UnionTypeDefinition t) -> t.description
  AST.TypeDefinition_EnumTypeDefinition (AST.EnumTypeDefinition t) -> t.description
  AST.TypeDefinition_InputObjectTypeDefinition (AST.InputObjectTypeDefinition t) -> t.description

getTypeDefFields :: AST.TypeDefinition -> Maybe AST.FieldsDefinition
getTypeDefFields = case _ of
  AST.TypeDefinition_ScalarTypeDefinition (AST.ScalarTypeDefinition _) -> Nothing
  AST.TypeDefinition_ObjectTypeDefinition (AST.ObjectTypeDefinition t) -> t.fieldsDefinition
  AST.TypeDefinition_InterfaceTypeDefinition (AST.InterfaceTypeDefinition t) -> t.fieldsDefinition
  AST.TypeDefinition_UnionTypeDefinition (AST.UnionTypeDefinition _) -> Nothing
  AST.TypeDefinition_EnumTypeDefinition (AST.EnumTypeDefinition _) -> Nothing
  AST.TypeDefinition_InputObjectTypeDefinition (AST.InputObjectTypeDefinition _) -> Nothing

getTypeDefKind :: AST.TypeDefinition -> ITypes.ITypeKind
getTypeDefKind = case _ of
  AST.TypeDefinition_ScalarTypeDefinition _ -> ITypes.SCALAR
  AST.TypeDefinition_ObjectTypeDefinition _ -> ITypes.OBJECT
  AST.TypeDefinition_InterfaceTypeDefinition _ -> ITypes.INTERFACE
  AST.TypeDefinition_UnionTypeDefinition _ -> ITypes.UNION
  AST.TypeDefinition_EnumTypeDefinition _ -> ITypes.ENUM
  AST.TypeDefinition_InputObjectTypeDefinition _ -> ITypes.INPUT_OBJECT

getTypeDefPossibleValues :: AST.TypeDefinition -> Maybe (List AST.NamedType)
getTypeDefPossibleValues = case _ of
  AST.TypeDefinition_ScalarTypeDefinition _ -> Nothing
  AST.TypeDefinition_ObjectTypeDefinition _ -> Nothing
  AST.TypeDefinition_InterfaceTypeDefinition _ -> Nothing
  AST.TypeDefinition_UnionTypeDefinition (AST.UnionTypeDefinition t) -> map unwrap t.unionMemberTypes
  AST.TypeDefinition_EnumTypeDefinition _ -> Nothing
  AST.TypeDefinition_InputObjectTypeDefinition _ -> Nothing
