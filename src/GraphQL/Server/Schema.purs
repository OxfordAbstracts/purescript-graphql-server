module GraphQL.Server.Schema where

import Prelude

import Data.GraphQL.AST as AST
import Data.GraphQL.AST.Print (class PrintAst)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import GraphQL.Resolver.Root (GqlRoot(..))
import GraphQL.Server.Schema.GetDefinitions (class GetDefinitions, getDefinitions)
import GraphQL.Server.Schema.Introspection.TypeName (class GqlTypeName)
import Type.Proxy (Proxy(..))

class PrintAst s <= GqlSchema a s | a -> s where
  gqlSchema :: a -> s

instance
  ( IsSymbol name
  , GqlTypeName q name
  , GetDefinitions q
  ) =>
  GqlSchema (GqlRoot q Unit) AST.Document where
  gqlSchema (GqlRoot { query }) = AST.Document $
    queryDefintion
      : gqlTypeDefs

    where
    queryDefintion = AST.Definition_TypeSystemDefinition $ AST.TypeSystemDefinition_SchemaDefinition $ AST.SchemaDefinition
      { directives: Nothing
      , rootOperationTypeDefinition:
          AST.RootOperationTypeDefinition
            ( { namedType: AST.NamedType $ reflectSymbol (Proxy :: Proxy name)
              , operationType: AST.Query
              }
            ) : Nil
      }

    gqlTypeDefs = getDefinitions query

