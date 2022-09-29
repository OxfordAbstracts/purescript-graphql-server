module GraphQL.Server.Schema.GetFieldsDefinitions where

import Prelude

import Data.GraphQL.AST as AST
import Data.List (reverse)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Data.Symbol (class IsSymbol, reflectSymbol)
import GraphQL.Record.Unsequence (class UnsequenceProxies, unsequenceProxies)
import GraphQL.Server.Schema.ToGqlType (class ToGqlType, toGqlType)
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Type.Proxy (Proxy(..))

class GetFieldsDefinitions :: forall k. k -> Constraint
class GetFieldsDefinitions a where
  getFieldsDefinitions :: Proxy a -> AST.FieldsDefinition

instance
  ( HFoldlWithIndex GetFieldDefsProps AST.FieldsDefinition { | p } AST.FieldsDefinition
  , UnsequenceProxies { | r } { | p }
  ) =>
  GetFieldsDefinitions { | r } where
  getFieldsDefinitions  r = getRecordTypeDefs ((unsequenceProxies r) :: { | p })

data GetFieldDefsProps = GetFieldDefsProps

instance
  ( IsSymbol label
  , ToGqlType a
  ) =>
  FoldingWithIndex GetFieldDefsProps (Proxy label) AST.FieldsDefinition (Proxy a) AST.FieldsDefinition where
  foldingWithIndex (GetFieldDefsProps) sym (AST.FieldsDefinition defs) _a = AST.FieldsDefinition $ (pure def) <> defs
    where
    def =
      AST.FieldDefinition
        { description: Nothing
        , name: reflectSymbol sym
        , argumentsDefinition: Nothing
        , type: toGqlType (Proxy :: Proxy a)
        , directives: Nothing
        }

getRecordTypeDefs
  :: forall r
   . HFoldlWithIndex GetFieldDefsProps AST.FieldsDefinition { | r } AST.FieldsDefinition
  => { | r }
  -> AST.FieldsDefinition
getRecordTypeDefs =
  hfoldlWithIndex GetFieldDefsProps (AST.FieldsDefinition mempty) >>> unwrap >>> reverse >>> wrap

