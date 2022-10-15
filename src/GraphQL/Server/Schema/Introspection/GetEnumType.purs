module GraphQL.Server.Schema.Introspection.GetEnumValues where

import Prelude

import Data.Generic.Rep (Constructor, NoArguments(..), Sum)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import GraphQL.Server.Schema.Introspection.Types (IEnumValue(..), IType(..), ITypeKind(..), defaultIType)
import Type.Proxy (Proxy(..))

enumType :: forall a. GetEnumValues a => String -> a -> IType
enumType typeName _ = IType defaultIType
  { kind = ENUM
  , name = Just typeName
  , enumValues = \_ -> Just $ getEnumValues (Proxy :: Proxy a) <#>
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

instance GetEnumValues a => GetEnumValues (Proxy a) where
  getEnumValues _ = getEnumValues (Proxy :: Proxy a)

instance IsSymbol name => GetEnumValues (Constructor name NoArguments) where
  getEnumValues _ = pure $ reflectSymbol (Proxy :: Proxy name)

instance (GetEnumValues a, GetEnumValues b) => GetEnumValues (Sum a b) where
  getEnumValues _ = getEnumValues (Proxy :: Proxy a) <> getEnumValues (Proxy :: Proxy b)
