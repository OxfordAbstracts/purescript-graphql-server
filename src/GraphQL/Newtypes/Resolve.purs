module GraphQL.Newtypes.Resolve where

import Prelude

import Data.Argonaut (Json)
import Effect.Aff (Aff)
import GraphQL.Newtypes.Result (Result)
import GraphQL.Parser.AST as AST
import GraphQL.Resolver.Untyped (Resolver(..))
import GraphQL.Resolver.Untyped as Untyped

-- data Query = QSelectionSet AST.SelectionSet

-- class Resolve m res where
--   resolve :: res -> Query -> m Result

-- instance Resolve (Aff Result) where
--   resolve 

class ToUntypedResolver a m where
  toUntypedResolver :: (a -> Json) -> a -> Untyped.Resolver m

instance Applicative m => ToUntypedResolver Int m where
  toUntypedResolver encoder i = Node $ pure $ encoder i