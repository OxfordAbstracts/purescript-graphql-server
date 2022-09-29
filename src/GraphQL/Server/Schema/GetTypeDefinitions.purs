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
import GraphQL.Server.Schema.GetFieldsDefinitions (class GetFieldsDefinitions, getFieldsDefinitions)
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
  , GetFieldsDefinitions { | r }
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



