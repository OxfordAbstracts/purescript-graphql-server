module GraphQL.Server.Schema where

import Prelude

import Data.GraphQL.AST as AST
import Data.GraphQL.AST.Print (class PrintAst)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import GraphQL.Resolver.Resolver.GqlObject (GqlObj)
-- import GraphQL.Server.Schema.GetFieldsDefinition (FieldsDefinitions(..))
import GraphQL.Server.Schema.GetTypeDefinitions (class GetObjectTypeDefinitions, getObjectTypeDefinitions)
import GraphQL.Server.Schema.GqlTypeName (class GqlTypeName)
import Type.Proxy (Proxy(..))

-- class PrintAst s <= GqlSchema a s | a -> s where
--   gqlSchema :: a -> s

-- data GqlRoot q m = GqlRoot { query :: q, mutation :: m }

-- instance
--   ( IsSymbol name
--   , GqlTypeName q name
--   , GetTypeDefinitions q
--   ) =>
--   GqlSchema (GqlRoot q Void) AST.Document where
--   gqlSchema (GqlRoot { query }) = AST.Document $
--     queryDefintion
--       : gqlTypeDefs

--     where
--     queryDefintion = AST.Definition_TypeSystemDefinition $ AST.TypeSystemDefinition_SchemaDefinition $ AST.SchemaDefinition
--       { directives: Nothing
--       , rootOperationTypeDefinition:
--           AST.RootOperationTypeDefinition
--             ( { namedType: AST.NamedType $ reflectSymbol (Proxy :: Proxy name)
--               , operationType: AST.Query
--               }
--             ) : Nil
--       }


--     -- inputDefinitions = AST.Definition_TypeSystemDefinition
--     --   $ AST.TypeSystemDefinition_TypeDefinition
--     --   $ AST.TypeDefinition_ObjectTypeDefinition
--     --   $ AST.ObjectTypeDefinition ?d

--     -- (TypeDefs typeDefs) = getTypeDefinitions query

--     gqlTypeDefs = getTypeDefinitions query # ?d
--     -- typeDefs <#> \{name, fieldDefinitions: FieldsDefinitions fieldDefinitions} -> AST.Definition_TypeSystemDefinition
--     --   $ AST.TypeSystemDefinition_TypeDefinition
--     --   $ AST.TypeDefinition_ObjectTypeDefinition
--     --   $ AST.ObjectTypeDefinition 
--     --   $  
--     --       { description  : Nothing
--     --     , directives  : Nothing
--     --     , fieldsDefinition  : Just $ AST.FieldsDefinition $ fieldDefinitions <#> \{label, gqlType} -> AST.FieldDefinition 
--     --           { argumentsDefinition: Nothing :: Maybe AST.ArgumentsDefinition
--     --           , description: Nothing :: Maybe String
--     --           , directives: Nothing :: Maybe AST.Directives
--     --           , name: label
--     --           , type: gqlType
--     --           }
--     --     , implementsInterfaces  : Nothing
--     --     , name
--     --     }


-- instance GqlSchema Boolean AST.BooleanValue where
--   gqlSchema = AST.BooleanValue

-- instance GqlSchema Int AST.IntValue where
--   gqlSchema = AST.IntValue

-- instance GqlSchema Number AST.FloatValue where
--   gqlSchema = AST.FloatValue

-- instance GqlSchema String AST.StringValue where
--   gqlSchema = AST.StringValue

-- -- instance GqlSchema Int where
-- --   gqlSchema _ = "Int"

-- -- instance GqlSchema Number where
-- --   gqlSchema _ = "Float"

-- -- instance GqlSchema String where
-- --   gqlSchema _ = "String"

-- -- instance name :: Class Type