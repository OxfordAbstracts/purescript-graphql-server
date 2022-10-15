module GraphQL.Server.Schema.Introspection.GetTypes where

import Data.List (List)
import GraphQL.Server.Schema.Introspection.Types (IType(..))

-- import Prelude

-- import Data.Generic.Rep (class Generic, Argument(..), Constructor, from)
-- import Data.List (List(..))
-- import Data.Maybe (Maybe(..))
-- import GraphQL.Resolver.Root (GqlRoot(..))
-- import GraphQL.Server.Schema.Introspection.GetType (class GetIType, getIType)
-- import GraphQL.Server.Schema.Introspection.Types (ISchema(..), IType(..), IType_T, defaultIType)
-- import GraphQL.Server.Schema.Introspection.Types as IT
-- import Safe.Coerce (coerce)
-- import Type.Proxy (Proxy(..))
-- import Unsafe.Coerce (unsafeCoerce)

-- class GetITypes a where
--   getITypes :: Proxy a -> List IType

-- instance GetIType a => GetITypes a where
--   getITypes = pure <<< getIType

-- genericGetITypes :: forall a name r. 
--   Generic a (Constructor name (Argument {|r})) => 
--   Proxy a -> 
--   List IType
-- genericGetITypes proxy = Nil

-- getITypes :: IType -> List ITypes