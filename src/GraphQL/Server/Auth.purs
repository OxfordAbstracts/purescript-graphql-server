module GraphQL.Server.Auth where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import GraphQL.Server.Branch (Branch(..))
import GraphQL.Server.Introspection.Types (IType(..), modifyIType, unnamed)
import GraphQL.Server.Introspection.Types as IT

type Auth pred = Branch pred Forbidden

auth :: forall pred a. pred -> a -> Branch pred Forbidden a
auth pred = Branch pred Forbidden

data Forbidden = Forbidden

forbiddenType :: IType
forbiddenType = unnamed IT.SCALAR # modifyIType _
  { name = Just forbiddenName
  }

forbiddenName :: String
forbiddenName = "!FORBIDDEN!"

isForbidden :: IType -> Boolean
isForbidden (IType t) = t.name == Just forbiddenName || maybe false isForbidden t.ofType
