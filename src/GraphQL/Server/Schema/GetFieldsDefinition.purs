module GraphQL.Server.Schema.GetFieldsDefinition (class GetFieldsDefinition, GetFieldsDefinitionProps, getFieldsDefinition) where

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

class GetFieldsDefinition :: forall k. k -> Constraint
class GetFieldsDefinition a where
  getFieldsDefinition :: Proxy a -> AST.FieldsDefinition

instance
  ( HFoldlWithIndex GetFieldsDefinitionProps AST.FieldsDefinition { | p } AST.FieldsDefinition
  , UnsequenceProxies { | r } { | p }
  ) =>
  GetFieldsDefinition { | r } where
  getFieldsDefinition r = getRecordTypeDefs ((unsequenceProxies r) :: { | p })

data GetFieldsDefinitionProps = GetFieldsDefinitionProps

instance
  ( IsSymbol label
  , ToGqlType a
  , GetArgumentsDefinitionFromFn a
  ) =>
  FoldingWithIndex GetFieldsDefinitionProps (Proxy label) AST.FieldsDefinition (Proxy a) AST.FieldsDefinition where
  foldingWithIndex (GetFieldsDefinitionProps) sym (AST.FieldsDefinition defs) _a = AST.FieldsDefinition $ def : defs
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
   . HFoldlWithIndex GetFieldsDefinitionProps AST.FieldsDefinition { | r } AST.FieldsDefinition
  => { | r }
  -> AST.FieldsDefinition
getRecordTypeDefs =
  hfoldlWithIndex GetFieldsDefinitionProps (AST.FieldsDefinition mempty) >>> unwrap >>> reverse >>> wrap

