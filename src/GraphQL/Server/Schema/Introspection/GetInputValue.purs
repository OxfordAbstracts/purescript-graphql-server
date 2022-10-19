module GraphQL.Server.Schema.Introspection.GetInputValue where

import Prelude

import Data.List (List(..))
import GraphQL.Server.Schema.Introspection.Types (IInputValue(..))
import Type.Proxy (Proxy)

class GetIInputValues a where
  getIInputValues :: Proxy a -> List IInputValue


instance GetIInputValues a where 
 getIInputValues _ = Nil