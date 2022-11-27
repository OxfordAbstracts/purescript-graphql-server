module GraphQL.Resolver.Path where

import Prelude

import Data.Argonaut (Json, encodeJson)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Show.Generic (genericShow)



type Path = List PathPart

data PathPart = Field String | Index Int


encodePath :: Path -> Json
encodePath = encodeJson <<< map case _ of 
  Field n -> encodeJson n
  Index i -> encodeJson i

derive instance Eq PathPart

derive instance Generic PathPart _

instance Show PathPart where
  show = genericShow
  