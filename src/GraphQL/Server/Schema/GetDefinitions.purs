module GraphQL.Server.Schema.GetDefinitions where

import Prelude

import Data.GraphQL.AST as AST
import Data.List (List)
import GraphQL.Server.Schema.GetTypeDefinitions (class GetInputObjectTypeDefinitionsFromFn, class GetObjectTypeDefinitions, getInputObjectTypeDefinitionsFromFn, getObjectTypeDefinitions)

class GetDefinitions a where
  getDefinitions :: a -> List AST.Definition

instance (GetObjectTypeDefinitions a, GetInputObjectTypeDefinitionsFromFn a) => GetDefinitions a where
  getDefinitions a =
    map objectToDef (getObjectTypeDefinitions a)
      <> map inputToDef (getInputObjectTypeDefinitionsFromFn a)
    where
    objectToDef =
      AST.Definition_TypeSystemDefinition
        <<< AST.TypeSystemDefinition_TypeDefinition
        <<< AST.TypeDefinition_ObjectTypeDefinition

    inputToDef = AST.Definition_TypeSystemDefinition
      <<< AST.TypeSystemDefinition_TypeDefinition
      <<< AST.TypeDefinition_InputObjectTypeDefinition