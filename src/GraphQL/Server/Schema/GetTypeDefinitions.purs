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
import GraphQL.Server.Schema.GetFieldsDefinition (class GetFieldsDefinition, getFieldsDefinition)
import GraphQL.Server.Schema.GetInputFieldsDefinition (class GetInputFieldsDefinition, getInputFieldsDefinition)
import Heterogeneous.Folding (class Folding, class HFoldl, hfoldl)
import Prim.RowList as RL
import Type.Proxy (Proxy(..))

class GetObjectTypeDefinitions a where
  getObjectTypeDefinitions :: a -> List AST.ObjectTypeDefinition

instance
  ( GetObjectTypeDefinitions (Proxy (GqlObj name { | r }))
  ) =>
  GetObjectTypeDefinitions (GqlObj name { | r }) where
  getObjectTypeDefinitions _gqlObj = getObjectTypeDefinitions (Proxy :: Proxy (GqlObj name { | r }))
else instance
  ( IsSymbol name
  , GetFieldsDefinition { | r }
  , UnsequenceProxies { | r } { | p }
  , HFoldl GetObjectTypeDefinitionsProps (List AST.ObjectTypeDefinition) { | p } (List AST.ObjectTypeDefinition)
  ) =>
  GetObjectTypeDefinitions (Proxy (GqlObj name { | r })) where
  getObjectTypeDefinitions _gqlObj =
    def : getObjectTypeDefintionsRecord ((unsequenceProxies (Proxy :: Proxy { | r })) :: { | p })
    where
    def =
      AST.ObjectTypeDefinition
        { description: Nothing
        , directives: Nothing
        , fieldsDefinition: Just $ getFieldsDefinition (Proxy :: Proxy { | r })
        , implementsInterfaces: Nothing
        , name: reflectSymbol (Proxy :: Proxy name)
        }
else instance GetObjectTypeDefinitions a where
  getObjectTypeDefinitions _ = Nil

data GetObjectTypeDefinitionsProps = GetObjectTypeDefinitionsProps

instance (GetObjectTypeDefinitions a) => Folding GetObjectTypeDefinitionsProps (List AST.ObjectTypeDefinition) a (List AST.ObjectTypeDefinition) where
  folding (GetObjectTypeDefinitionsProps) defs a = defs <> getObjectTypeDefinitions a

getObjectTypeDefintionsRecord
  :: forall r
   . HFoldl GetObjectTypeDefinitionsProps
       (List AST.ObjectTypeDefinition)
       { | r }
       (List AST.ObjectTypeDefinition)
  => { | r }
  -> (List AST.ObjectTypeDefinition)
getObjectTypeDefintionsRecord = hfoldl GetObjectTypeDefinitionsProps (Nil :: List AST.ObjectTypeDefinition)

class GetInputObjectTypeDefinitions a where
  getInputObjectTypeDefinitions
    :: a -> List AST.InputObjectTypeDefinition

instance
  ( GetInputObjectTypeDefinitions (Proxy (GqlObj name { | r }))
  ) =>
  GetInputObjectTypeDefinitions (GqlObj name { | r }) where
  getInputObjectTypeDefinitions _gqlObj = getInputObjectTypeDefinitions (Proxy :: Proxy (GqlObj name { | r }))
else instance
  ( IsSymbol name
  , RL.RowToList row rl
  , GetInputFieldsDefinition { | r }
  , UnsequenceProxies { | r } { | p }
  , HFoldl GetInputObjectTypeDefinitionsProps (List AST.InputObjectTypeDefinition) { | p } (List AST.InputObjectTypeDefinition)
  ) =>
  GetInputObjectTypeDefinitions (Proxy (GqlObj name { | r })) where
  getInputObjectTypeDefinitions _gqlObj = def : getInputObjectTypeDefintionsRecord ((unsequenceProxies (Proxy :: Proxy { | r })) :: { | p })
    where
    def =
      AST.InputObjectTypeDefinition
        { description: Nothing
        , directives: Nothing
        , inputFieldsDefinition: Just $ getInputFieldsDefinition (Proxy :: Proxy { | r })
        , name: reflectSymbol (Proxy :: Proxy name)
        }
else instance GetInputObjectTypeDefinitions a where
  getInputObjectTypeDefinitions _ = Nil

data GetInputObjectTypeDefinitionsProps = GetInputObjectTypeDefinitionsProps

instance (GetInputObjectTypeDefinitions a) => Folding GetInputObjectTypeDefinitionsProps (List AST.InputObjectTypeDefinition) a (List AST.InputObjectTypeDefinition) where
  folding (GetInputObjectTypeDefinitionsProps) defs a = defs <> getInputObjectTypeDefinitions a

getInputObjectTypeDefintionsRecord
  :: forall r
   . HFoldl GetInputObjectTypeDefinitionsProps
       (List AST.InputObjectTypeDefinition)
       { | r }
       (List AST.InputObjectTypeDefinition)
  => { | r }
  -> (List AST.InputObjectTypeDefinition)
getInputObjectTypeDefintionsRecord = hfoldl GetInputObjectTypeDefinitionsProps (Nil :: List AST.InputObjectTypeDefinition)

