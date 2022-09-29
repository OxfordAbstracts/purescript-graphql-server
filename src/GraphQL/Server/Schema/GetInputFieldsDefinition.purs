module GraphQL.Server.Schema.GetInputFieldsDefinition (class GetInputFieldsDefinition, GetInputFieldsDefinitionProps, getInputFieldsDefinition) where

import Prelude

import Data.GraphQL.AST as AST
import Data.List (reverse, (:))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Data.Symbol (class IsSymbol, reflectSymbol)
import GraphQL.Record.Unsequence (class UnsequenceProxies, unsequenceProxies)
import GraphQL.Server.Schema.GetArgumentsDefinition (class GetArgumentsDefinitionFromFn)
import GraphQL.Server.Schema.ToGqlType (class ToGqlType, toGqlType)
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Type.Proxy (Proxy(..))

class GetInputFieldsDefinition :: forall k. k -> Constraint
class GetInputFieldsDefinition a where
  getInputFieldsDefinition :: Proxy a -> AST.InputFieldsDefinition

instance
  ( HFoldlWithIndex GetInputFieldsDefinitionProps AST.InputFieldsDefinition { | p } AST.InputFieldsDefinition
  , UnsequenceProxies { | r } { | p }
  ) =>
  GetInputFieldsDefinition { | r } where
  getInputFieldsDefinition r = getRecordTypeDefs ((unsequenceProxies r) :: { | p })

data GetInputFieldsDefinitionProps = GetInputFieldsDefinitionProps

instance
  ( IsSymbol label
  , ToGqlType a
  , GetArgumentsDefinitionFromFn a
  ) =>
  FoldingWithIndex GetInputFieldsDefinitionProps (Proxy label) AST.InputFieldsDefinition (Proxy a) AST.InputFieldsDefinition where
  foldingWithIndex (GetInputFieldsDefinitionProps) sym (AST.InputFieldsDefinition defs) _a = AST.InputFieldsDefinition $ def : defs
    where
    def =
      AST.InputValueDefinition
        { description: Nothing
        , defaultValue: Nothing
        , name: reflectSymbol sym
        , type: toGqlType (Proxy :: Proxy a)
        , directives: Nothing
        }

getRecordTypeDefs
  :: forall r
   . HFoldlWithIndex GetInputFieldsDefinitionProps AST.InputFieldsDefinition { | r } AST.InputFieldsDefinition
  => { | r }
  -> AST.InputFieldsDefinition
getRecordTypeDefs =
  hfoldlWithIndex GetInputFieldsDefinitionProps (AST.InputFieldsDefinition mempty) >>> unwrap >>> reverse >>> wrap

