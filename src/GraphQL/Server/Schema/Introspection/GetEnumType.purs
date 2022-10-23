module GraphQL.Server.Schema.Introspection.GetEnumValues where

import Prelude

import Data.Generic.Rep (class Generic, Constructor, NoArguments, Sum)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import GraphQL.GqlRep (class GqlRep, GEnum)
import GraphQL.Server.Schema.Introspection.Types (IEnumValue(..), IType(..), ITypeKind(..), defaultIType)
import Type.Proxy (Proxy(..))

enumType
  :: forall a rep name
   . IsSymbol name
  => Generic a rep
  => GqlRep a GEnum name
  => GetEnumValues rep
  => Proxy a
  -> IType
enumType _ = IType defaultIType
  { kind = ENUM
  , name = Just $ reflectSymbol (Proxy :: Proxy name)
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
