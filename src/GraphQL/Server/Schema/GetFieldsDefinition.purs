module GraphQL.Server.Schema.GetFieldsDefinitions (class GetFieldsDefinitions, GetFieldsDefinitionsProps, getFieldsDefinitions) where

import Prelude

import Data.GraphQL.AST as AST
import Data.List (reverse, (:))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Data.Symbol (class IsSymbol, reflectSymbol)
import GraphQL.Record.Unsequence (class UnsequenceProxies, unsequenceProxies)
import GraphQL.Server.Schema.GetArgumentsDefinition (class GetArgumentsDefinitionFromFn, getArgumentsDefinitionFromFn)
import GraphQL.Server.Schema.ToGqlType (class ToGqlType, toGqlType)
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Type.Proxy (Proxy(..))

class GetFieldsDefinitions :: forall k. k -> Constraint
class GetFieldsDefinitions a where
  getFieldsDefinitions :: Proxy a -> AST.FieldsDefinition

instance
  ( HFoldlWithIndex GetFieldsDefinitionsProps AST.FieldsDefinition { | p } AST.FieldsDefinition
  , UnsequenceProxies { | r } { | p }
  ) =>
  GetFieldsDefinitions { | r } where
  getFieldsDefinitions r = getRecordTypeDefs ((unsequenceProxies r) :: { | p })

data GetFieldsDefinitionsProps = GetFieldsDefinitionsProps

instance
  ( IsSymbol label
  , ToGqlType a
  , GetArgumentsDefinitionFromFn a
  ) =>
  FoldingWithIndex GetFieldsDefinitionsProps (Proxy label) AST.FieldsDefinition (Proxy a) AST.FieldsDefinition where
  foldingWithIndex (GetFieldsDefinitionsProps) sym (AST.FieldsDefinition defs) _a = AST.FieldsDefinition $ def : defs
    where
    def =
      AST.FieldDefinition
        { description: Nothing
        , argumentsDefinition: getArgumentsDefinitionFromFn (Proxy :: Proxy a)
        , name: reflectSymbol sym
        , type: toGqlType (Proxy :: Proxy a)
        , directives: Nothing
        }

getRecordTypeDefs
  :: forall r
   . HFoldlWithIndex GetFieldsDefinitionsProps AST.FieldsDefinition { | r } AST.FieldsDefinition
  => { | r }
  -> AST.FieldsDefinition
getRecordTypeDefs =
  hfoldlWithIndex GetFieldsDefinitionsProps (AST.FieldsDefinition mempty) >>> unwrap >>> reverse >>> wrap

