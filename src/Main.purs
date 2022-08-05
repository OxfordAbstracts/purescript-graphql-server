module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import GraphQL.Server (start)

main :: Effect Unit
main = do
  void $ start 1
