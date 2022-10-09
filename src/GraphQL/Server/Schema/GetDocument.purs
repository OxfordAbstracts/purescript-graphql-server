module GraphQL.Server.Schema.GetDocument where

import Prelude

import Data.GraphQL.AST as AST
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import GraphQL.Resolver.Root (GqlRoot(..))
import GraphQL.Server.Schema.GetDefinitions (class GetDefinitions, getDefinitions)
import GraphQL.Server.Schema.Introspection.TypeName (class GqlTypeName)
import Type.Proxy (Proxy(..))

class GetDocument root where
  getDocument :: root -> AST.Document

instance
  ( IsSymbol name
  , GqlTypeName q name
  , GetDefinitions q
  ) =>
  GetDocument (GqlRoot q Unit) where
  getDocument (GqlRoot { query }) = AST.Document $
    getNamedDefinition (Proxy :: Proxy name)
      : gqlTypeDefs
    where
    gqlTypeDefs = getDefinitions query
else instance
  ( IsSymbol qName
  , GqlTypeName q qName
  , IsSymbol mName
  , GqlTypeName m mName
  , GetDefinitions q
  , GetDefinitions m
  ) =>
  GetDocument (GqlRoot q m) where
  getDocument (GqlRoot { query }) = AST.Document $
    getNamedDefinition (Proxy :: Proxy qName)
      : getNamedDefinition (Proxy :: Proxy mName)
      : gqlTypeDefs
    where
    gqlTypeDefs = getDefinitions query

getNamedDefinition :: forall sym. IsSymbol sym => Proxy sym -> AST.Definition
getNamedDefinition nameProxy =
  AST.Definition_TypeSystemDefinition $ AST.TypeSystemDefinition_SchemaDefinition $ getSchemaDefinition nameProxy

getSchemaDefinition :: forall sym. IsSymbol sym => Proxy sym -> AST.SchemaDefinition
getSchemaDefinition nameProxy = AST.SchemaDefinition
  { directives: Nothing
  , rootOperationTypeDefinition:
      AST.RootOperationTypeDefinition
        ( { namedType: AST.NamedType $ reflectSymbol nameProxy
          , operationType: AST.Query
          }
        ) : Nil
  }


-- getNamedDefinition = AST.Definition_TypeSystemDefinition $ AST.TypeSystemDefinition_SchemaDefinition $ AST.SchemaDefinition
--       { directives: Nothing
--       , rootOperationTypeDefinition:
--           AST.RootOperationTypeDefinition
--             ( { namedType: AST.NamedType $ reflectSymbol (Proxy :: Proxy name)
--               , operationType: AST.Query
--               }
--             ) : Nil
--       }