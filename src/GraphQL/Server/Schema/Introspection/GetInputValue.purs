module GraphQL.Server.Schema.Introspection.GetInputValue where

import Prelude

import GraphQL.Server.Schema.Introspection.Types (IInputValue(..))

class GetIInputValue a where 
  getIInputValue :: a -> IInputValue



  