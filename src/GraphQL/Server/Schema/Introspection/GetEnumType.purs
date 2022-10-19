module GraphQL.Server.Schema.Introspection.GetEnumValues where

import Prelude

import Data.Generic.Rep (class Generic, Constructor, NoArguments, Sum, to)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import GraphQL.Server.Schema.Introspection.Types (IEnumValue(..), IType(..), ITypeKind(..), defaultIType)
import Type.Proxy (Proxy(..))

enumType :: forall a rep. Generic a rep => GetEnumValues rep => String -> Proxy a -> IType
enumType typeName _ = IType defaultIType
  { kind = ENUM
  , name = Just typeName
  , enumValues = \_ -> Just $ getEnumValues (Proxy :: Proxy rep) <#>
      IEnumValue <<<
        { name: _
        , description: Nothing
        , isDeprecated: false
        , deprecationReason: Nothing
        }
  }

class GetEnumValues :: forall k. k -> Constraint
class GetEnumValues a where
  getEnumValues :: Proxy a -> List String

instance IsSymbol name => GetEnumValues (Constructor name NoArguments) where
  getEnumValues _ = pure $ reflectSymbol (Proxy :: Proxy name)

instance (GetEnumValues a, GetEnumValues b) => GetEnumValues (Sum a b) where
  getEnumValues _ = getEnumValues (Proxy :: Proxy a) <> getEnumValues (Proxy :: Proxy b)
