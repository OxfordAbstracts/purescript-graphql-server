module GraphQL.Server.Schema.GetArgumentsDefinition where

import Prelude

import Data.GraphQL.AST as AST
import Data.List (reverse, (:))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Data.Symbol (class IsSymbol, reflectSymbol)
import GraphQL.Record.Unsequence (class UnsequenceProxies, unsequenceProxies)
import GraphQL.Server.Schema.ToGqlType (class ToGqlType, toGqlType)
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Type.Proxy (Proxy(..))

class GetArgumentsDefinitionFromFn :: forall k. k -> Constraint
class GetArgumentsDefinitionFromFn a where
  getArgumentsDefinitionFromFn :: Proxy a -> Maybe AST.ArgumentsDefinition


instance GetArgumentsDefinition a => GetArgumentsDefinitionFromFn (a -> b) where
  getArgumentsDefinitionFromFn _ = Just $ getArgumentsDefinition (Proxy :: Proxy a)
else instance GetArgumentsDefinitionFromFn a where 
  getArgumentsDefinitionFromFn _ = Nothing


class GetArgumentsDefinition :: forall k. k -> Constraint
class GetArgumentsDefinition a where
  getArgumentsDefinition :: Proxy a -> AST.ArgumentsDefinition


instance
  ( HFoldlWithIndex GetArgumentsDefinitionProps AST.ArgumentsDefinition { | p } AST.ArgumentsDefinition
  , UnsequenceProxies { | r } { | p }
  ) =>
  GetArgumentsDefinition { | r } where
  getArgumentsDefinition r = getRecordTypeDefs ((unsequenceProxies r) :: { | p })
-- instance 
data GetArgumentsDefinitionProps = GetArgumentsDefinitionProps

instance
  ( IsSymbol label
  , ToGqlType a
  ) =>
  FoldingWithIndex GetArgumentsDefinitionProps (Proxy label) AST.ArgumentsDefinition (Proxy a) AST.ArgumentsDefinition where
  foldingWithIndex (GetArgumentsDefinitionProps) sym (AST.ArgumentsDefinition defs) _a = AST.ArgumentsDefinition $  def : defs
    where
    def =
      AST.InputValueDefinition
        { defaultValue: Nothing
        , description: Nothing
        , directives: Nothing
        , name: reflectSymbol sym
        , type: toGqlType (Proxy :: Proxy a)
        }

getRecordTypeDefs
  :: forall r
   . HFoldlWithIndex GetArgumentsDefinitionProps AST.ArgumentsDefinition { | r } AST.ArgumentsDefinition
  => { | r }
  -> AST.ArgumentsDefinition
getRecordTypeDefs =
  hfoldlWithIndex GetArgumentsDefinitionProps (AST.ArgumentsDefinition mempty) >>> unwrap >>> reverse >>> wrap

