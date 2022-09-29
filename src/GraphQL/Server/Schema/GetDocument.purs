module GraphQL.Server.Schema.GetDocument where


import Prelude

import Data.GraphQL.AST as AST
import Data.List (reverse, (:))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Data.Symbol (class IsSymbol, reflectSymbol)
import GraphQL.Record.Unsequence (class UnsequenceProxies, unsequenceProxies)
import GraphQL.Server.Schema.GetArgumentsDefinition (class GetArgumentsDefinitionFromFn, getArgumentsDefinitionFromFn)
import GraphQL.Server.Schema.GetTypeDefinitions (class GetObjectTypeDefinitions)
import GraphQL.Server.Schema.ToGqlType (class ToGqlType, toGqlType)
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Type.Proxy (Proxy(..))

class GetDocument :: forall k. k -> Constraint
class GetDocument a where
  getDocument :: Proxy a -> AST.Document

instance
  ( HFoldlWithIndex GetDocumentProps AST.Document { | p } AST.Document
  , UnsequenceProxies { | r } { | p }
  ) =>
  GetDocument { | r } where
  getDocument r = getRecordTypeDefs ((unsequenceProxies r) :: { | p })

data GetDocumentProps = GetDocumentProps

instance
  ( IsSymbol label
  , GetObjectTypeDefinitions a
  ) =>
  FoldingWithIndex GetDocumentProps (Proxy label) AST.Document (Proxy a) AST.Document where
  foldingWithIndex (GetDocumentProps) sym (AST.Document defs) _a = 
    -- AST.Document $ def : defs
    AST.Document  defs
    -- where
    -- def =
    --   AST.Definition_TypeSystemDefinition 
    --    $ AST.TypeSystemDefinition_TypeDefinition ?d
          -- { description: Nothing
          -- , argumentsDefinition: getArgumentsDefinitionFromFn (Proxy :: Proxy a)
          -- , name: reflectSymbol sym
          -- , type: toGqlType (Proxy :: Proxy a)
          -- , directives: Nothing
          -- }

getRecordTypeDefs
  :: forall r
   . HFoldlWithIndex GetDocumentProps AST.Document { | r } AST.Document
  => { | r }
  -> AST.Document
getRecordTypeDefs =
  hfoldlWithIndex GetDocumentProps (AST.Document mempty) >>> unwrap >>> reverse >>> wrap




