module GraphQL.Server.Schema.Introspection where

import Prelude

import Data.Generic.Rep (class Generic, Argument, Constructor, NoArguments(..), Product, Sum, from)
import Data.Map (lookup)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import GraphQL.Resolver.JsonResolver (Resolver)
import GraphQL.Resolver.ToResolver (toResolver)
import GraphQL.Server.MaxDepth (maxIntrospectionDepth)
import GraphQL.Server.Schema.Introspection.Types (ISchema(..), IType(..))
import Type.Proxy (Proxy(..))

makeIntrospectionResolver :: forall n m. Applicative m => ISchema -> Resolver m
makeIntrospectionResolver schema@(ISchema { types }) = toResolver maxIntrospectionDepth introspection
  where
  introspection = Introspection
    { __schema: schema
    , __type: \{ name } -> lookup (Just name) typeMap
    }

  typeMap = Map.fromFoldable $ types <#> \iType@(IType { name }) -> Tuple name iType


-- test1 :: forall n m. Applicative m => ISchema -> Resolver m
-- test1 schema@(ISchema { types }) = toResolver maxDepth introspection

newtype Introspection = Introspection
  { __schema :: ISchema
  , __type :: { name :: String } -> Maybe IType
  }

derive instance Generic Introspection _

-- x
--   :: Proxy
--        ( Sum (Constructor "A" (Argument Int))
--            ( Sum
--                ( Constructor "B"
--                    ( Product
--                        (Argument String)
--                        (Argument Number)
--                    )
--                )
--                (Constructor "C" NoArguments)
--            )
--        )
x
  :: Proxy
       ( Sum (Constructor "A" (Argument Int))
           ( Sum
               ( Constructor "B"
                   ( Product (Argument String)
                       (Product (Argument Number) (Argument Int))
                   )
               )
               (Constructor "C" NoArguments)
           )
       )
x = map from (Proxy :: Proxy T)

data T = A Int | B String Number Int | C

derive instance Generic T _