module GraphQL.Server.Schema.GetTypeDefinitions
  ( GetObjectTypeDefinitionsProps(..)
  , class GetObjectTypeDefinitions
  , getObjectTypeDefinitions
  ) where

import Prelude

import Data.GraphQL.AST as AST
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import GraphQL.Record.Unsequence (class UnsequenceProxies, unsequenceProxies)
import GraphQL.Resolver.Resolver.GqlObject (GqlObj)
import GraphQL.Server.Schema.GetFieldsDefinition (class GetFieldsDefinition, getFieldsDefinitions)
import GraphQL.Server.Schema.GetInputFieldsDefinition (class GetInputFieldsDefinition, getInputFieldsDefinitions)
import Heterogeneous.Folding (class Folding, class HFoldl, hfoldl)
import Prim.RowList as RL
import Type.Proxy (Proxy(..))

class GetObjectTypeDefinitions :: forall k. k -> Constraint
class GetObjectTypeDefinitions a where
  getObjectTypeDefinitions
    :: Proxy a
    -> List AST.ObjectTypeDefinition

instance
  ( IsSymbol name
  , RL.RowToList row rl
  , GetFieldsDefinition { | r }
  , UnsequenceProxies { | r } { | p }
  , HFoldl GetObjectTypeDefinitionsProps (List AST.ObjectTypeDefinition) { | p } (List AST.ObjectTypeDefinition)
  ) =>
  GetObjectTypeDefinitions (GqlObj name { | r }) where
  getObjectTypeDefinitions _gqlObj = def : getObjectTypeDefintionsRecord ((unsequenceProxies (Proxy :: Proxy { | r })) :: { | p })
    where
    def = AST.ObjectTypeDefinition
      { description: Nothing
      , directives: Nothing
      , fieldsDefinition: Just $ getFieldsDefinitions (Proxy :: Proxy { | r })
      , implementsInterfaces: Nothing
      , name: reflectSymbol (Proxy :: Proxy name)
      }

data GetObjectTypeDefinitionsProps = GetObjectTypeDefinitionsProps

instance (GetObjectTypeDefinitions a) => Folding GetObjectTypeDefinitionsProps (List AST.ObjectTypeDefinition) a (List AST.ObjectTypeDefinition) where
  folding (GetObjectTypeDefinitionsProps) defs _a = defs <> getObjectTypeDefinitions (Proxy :: Proxy a)

getObjectTypeDefintionsRecord
  :: forall r
   . HFoldl GetObjectTypeDefinitionsProps
       (List AST.ObjectTypeDefinition)
       { | r }
       (List AST.ObjectTypeDefinition)
  => { | r }
  -> (List AST.ObjectTypeDefinition)
getObjectTypeDefintionsRecord = hfoldl GetObjectTypeDefinitionsProps (Nil :: List AST.ObjectTypeDefinition)




class GetInputObjectTypeDefinitions :: forall k. k -> Constraint
class GetInputObjectTypeDefinitions a where
  getInputObjectTypeDefinitions
    :: Proxy a
    -> List AST.InputObjectTypeDefinition

instance
  ( IsSymbol name
  , RL.RowToList row rl
  , GetInputFieldsDefinition { | r }
  , UnsequenceProxies { | r } { | p }
  , HFoldl GetInputObjectTypeDefinitionsProps (List AST.InputObjectTypeDefinition) { | p } (List AST.InputObjectTypeDefinition)
  ) =>
  GetInputObjectTypeDefinitions (GqlObj name { | r }) where
  getInputObjectTypeDefinitions _gqlObj = def : getInputObjectTypeDefintionsRecord ((unsequenceProxies (Proxy :: Proxy { | r })) :: { | p })
    where
    def = AST.InputObjectTypeDefinition
      { description: Nothing
      , directives: Nothing
      , inputFieldsDefinition: Just $ getInputFieldsDefinitions (Proxy :: Proxy { | r })
      , name: reflectSymbol (Proxy :: Proxy name)
      }

data GetInputObjectTypeDefinitionsProps = GetInputObjectTypeDefinitionsProps

instance (GetInputObjectTypeDefinitions a) => Folding GetInputObjectTypeDefinitionsProps (List AST.InputObjectTypeDefinition) a (List AST.InputObjectTypeDefinition) where
  folding (GetInputObjectTypeDefinitionsProps) defs _a = defs <> getInputObjectTypeDefinitions (Proxy :: Proxy a)

getInputObjectTypeDefintionsRecord
  :: forall r
   . HFoldl GetInputObjectTypeDefinitionsProps
       (List AST.InputObjectTypeDefinition)
       { | r }
       (List AST.InputObjectTypeDefinition)
  => { | r }
  -> (List AST.InputObjectTypeDefinition)
getInputObjectTypeDefintionsRecord = hfoldl GetInputObjectTypeDefinitionsProps (Nil :: List AST.InputObjectTypeDefinition)



