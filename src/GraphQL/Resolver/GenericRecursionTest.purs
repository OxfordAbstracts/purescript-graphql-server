module GraphQL.Resolver.GenericRecursionTest where

import Prelude

import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), from)
import Data.Tuple (Tuple(..))
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)


class T a b   where 
  t :: a -> b 

instance T Unit Unit where 
  t = identity

else instance (Generic a (Constructor name (Argument ty)), T ty b) => T (Proxy a) (Tuple (Proxy name) (Proxy b))  where
  t a = Tuple (Proxy :: Proxy name) $ a <#> from <#> \(Constructor (Argument ty)) -> t ty

else instance (Generic a (Constructor name (Argument ty)), T ty b) => T a (Tuple (Proxy name) b)  where
  t a = Tuple (Proxy :: Proxy name) (t ty)
    where 
    (Constructor (Argument ty)) = from a


t1 :: Unit
t1 = t unit

newtype ADT1 = ADT1 Unit

derive instance Generic ADT1 _ 

g :: Constructor "ADT1" (Argument Unit)
g = from (ADT1 unit)

t2 :: Tuple (Proxy "ADT1") Unit
t2 = t (ADT1 unit)



newtype ADT2 = ADT2 (Proxy ADT3)

derive instance Generic ADT2 _ 

newtype ADT3 = ADT3 ADT2

derive instance Generic ADT3 _ 

t3 = t (ADT2 (Proxy :: Proxy ADT3))


