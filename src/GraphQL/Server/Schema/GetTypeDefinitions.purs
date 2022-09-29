module GraphQL.Server.Schema.GetTypeDefinitions
  ( GetGetObjectTypeDefinitionsProps(..)
  , class GetObjectTypeDefinitions
  , getTypeDefinitions
  ) where

import Prelude

import Data.GraphQL.AST as AST
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import GraphQL.Record.Unsequence (class UnsequenceProxies, unsequenceProxies)
import GraphQL.Resolver.Resolver.GqlObject (GqlObj)
import GraphQL.Server.Schema.GetFieldsDefinitions (class GetFieldsDefinitions, getFieldsDefinitions)
import Heterogeneous.Folding (class Folding, class HFoldl, hfoldl)
import Prim.RowList as RL
import Type.Proxy (Proxy(..))

class GetObjectTypeDefinitions :: forall k. k -> Constraint
class GetObjectTypeDefinitions a where
  getTypeDefinitions
    :: Proxy a
    -> List AST.ObjectTypeDefinition

instance
  ( IsSymbol name
  , RL.RowToList row rl
  , GetFieldsDefinitions { | r }
  , UnsequenceProxies { | r } { | p }
  , HFoldl GetGetObjectTypeDefinitionsProps (List AST.ObjectTypeDefinition) { | p } (List AST.ObjectTypeDefinition)
  ) =>
  GetObjectTypeDefinitions (GqlObj name { | r }) where
  getTypeDefinitions _gqlObj = def : getRecordTypeDefs ((unsequenceProxies (Proxy :: Proxy { | r })) :: { | p })
    where
    def = AST.ObjectTypeDefinition
      { description: Nothing
      , directives: Nothing
      , fieldsDefinition: Just $ getFieldsDefinitions (Proxy :: Proxy { | r })
      , implementsInterfaces: Nothing
      , name: reflectSymbol (Proxy :: Proxy name)
      }

data GetGetObjectTypeDefinitionsProps = GetGetObjectTypeDefinitionsProps

instance (GetObjectTypeDefinitions a) => Folding GetGetObjectTypeDefinitionsProps (List AST.ObjectTypeDefinition) a (List AST.ObjectTypeDefinition) where
  folding (GetGetObjectTypeDefinitionsProps) defs _a = defs <> getTypeDefinitions (Proxy :: Proxy a)

getRecordTypeDefs
  :: forall r
   . HFoldl GetGetObjectTypeDefinitionsProps
       (List AST.ObjectTypeDefinition)
       { | r }
       (List AST.ObjectTypeDefinition)
  => { | r }
  -> (List AST.ObjectTypeDefinition)
getRecordTypeDefs = hfoldl GetGetObjectTypeDefinitionsProps (Nil :: List AST.ObjectTypeDefinition)



