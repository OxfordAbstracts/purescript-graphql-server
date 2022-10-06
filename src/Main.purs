module Main where

import Prelude

import Data.Array (filter)
import Data.Maybe (Maybe, maybe)
import Data.Newtype (unwrap)
import Debug (spy, traceM)
import Effect (Effect)
import Effect.Aff (Aff, Fiber, joinFiber)
import Effect.Console (log)
import GraphQL.Resolver.GqlIo (GqlFiber, GqlIo(..))
import GraphQL.Resolver.Resolver.GqlObject (GqlObj(..))
import GraphQL.Server (start)
import GraphQL.Server.Schema (GqlRoot(..))
import Type.Proxy (Proxy(..))

main :: Effect Unit
main = do
  void $ start
    { root: GqlRoot resolvers
    }

  where
  resolvers =
    { mutation: unit
    , query:
        { book
        , books
        }
    }

  books = \(opts :: { maxPrice :: Maybe Number }) -> do
    -- let _ = spy "opts" opts
    io $
      filter (\b -> maybe true (b.price <= _) opts.maxPrice)
        [ book
        , { title: "Consider Phlebas"
          , price: 5.99
          , author: io author
          }
        ]

  book =
    { title: "State of the Art"
    , price: 9.99
    , author: io author
    }

  author = GqlObj
    { name: "Iain M. Banks"
    , bio: io "This is some stuff about the author"
    }

-- unitFn :: forall a. a -> (Unit -> a)

io :: forall a. a -> GqlFiber a
io = GqlIo <<< pure

