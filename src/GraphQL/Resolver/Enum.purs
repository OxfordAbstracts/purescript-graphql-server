module GraphQL.Resolver.Enum where

import Data.Generic.Rep (class Generic)
import Data.List (List)

class EnumResolver :: Type -> Symbol -> Constraint
class EnumResolver a sym | a -> sym where
  enumValues :: a -> List String

class EnumResolverGeneric a where
  genericEnumValues :: a -> List String

data Test = A | B | C (Int -> Int)

derive instance Generic Test _

-- x
--   :: Sum
--        ( Constructor
--            "A"
--            NoArguments
--        )
--        ( Sum
--            ( Constructor "B" NoArguments
--            )
--            (Constructor "C" NoArguments)
--        )
-- x = from A

-- data Test2 = A2

-- derive instance Generic Test2 _

-- x2 :: Constructor "A2" NoArguments
-- x2 = from A2