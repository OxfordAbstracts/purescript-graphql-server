module GraphQL.Resolver.GenericRecursionTestInt where

import Prelude

import Data.Maybe (Maybe, maybe)

-- import Prim.RowList (Cons, Nil)

data Cons :: forall k. Symbol -> Type -> k -> Type
data Cons sym fn rest = Cons fn

data Nil

class T memo a where
  t :: memo -> a -> Int

instance T memo Unit where
  t _ _ = 0

else instance T ((Cons "" (Maybe a -> Int) rest)) a => T ((Cons "" (Maybe a -> Int) rest)) (Maybe a) where
  t (Cons fn) = fn

else instance T memo a => T memo (Maybe a) where
  t m = maybe 0 (t m)

-- else instance T (Proxy a) where
--   t _a = 1

-- else instance  HFoldlWithIndex Props Int {|r} Int => T ({|r}) where
--   t _a =  tRec _a

-- else instance (Generic a (Constructor name (Argument ty)), T ty) => T a where
--   t a = 1 + t ty
--     where
--     (Constructor (Argument ty)) = from a

-- t1 :: Int
-- t1 = t unit

-- newtype ADT1 = ADT1 Unit

-- derive instance Generic ADT1 _

-- -- t2 :: Tuple (Proxy "ADT1") Unit
-- t2 :: Int
-- t2 = t (ADT1 unit)

-- newtype ADT2 = ADT2 (Proxy ADT3)

-- derive instance Generic ADT2 _

-- newtype ADT3 = ADT3 ADT2

-- derive instance Generic ADT3 _

-- t3 :: Int
-- t3 = t (ADT2 (Proxy :: Proxy ADT3))

-- data R r = R r

-- derive instance Generic (R r) _

-- data Props = Props

-- instance
--   ( IsSymbol sym
--   , T a
--   ) =>
--   FoldingWithIndex (Props) (Proxy sym) Int a Int where
--   foldingWithIndex (Props) _prop res a = res + t a

-- tRec :: forall r. HFoldlWithIndex Props Int r Int => r -> Int
-- tRec = hfoldlWithIndex Props 0

-- tr1 = tRec { a: unit, b: unit, c: Proxy :: Proxy Int }

-- newtype ADT4 = ADT4 { a :: Maybe ADT5}

-- derive instance Generic ADT4 _

-- newtype ADT5 = ADT5 { a :: Maybe ADT4 }

-- derive instance Generic ADT5 _

-- tr2 = t $ ADT4 {a : Nothing}

-- -- tr2 = tRec { a: ADT4 {}, b: ADT5 {} }
