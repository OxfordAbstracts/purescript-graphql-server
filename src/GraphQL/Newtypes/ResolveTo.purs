module GraphQL.Resolver.Newtypes.ResolveTo where

import Prelude

import Data.Argonaut (Json)
import Effect.Aff (Aff)
import GraphQL.Newtypes.Scalar (Scalar)
import Heterogeneous.Mapping (class HMap, class Mapping, hmap)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)


class ResolveTo :: forall k1 k2. k1 -> k2 -> Constraint
class ResolveTo resolver return | resolver -> return

-- instance ResolveTo (GqlIo m a) a
instance ResolveTo (Aff a) a 
instance (HMap ResolveToProp res ret) => ResolveTo (GqlObj sym res) ret
instance (ResolveTo a c) => ResolveTo (a -> b) c
instance ResolveTo (Scalar sym a) a
instance ResolveTo Boolean Boolean
instance ResolveTo Int Int
instance ResolveTo Number Number
instance ResolveTo String String
instance ResolveTo Json Json

-- Objects

newtype GqlObj :: forall k. k -> Type -> Type
newtype GqlObj name rec = GqlObj rec

-- Records 

data ResolveToProp = ResolveToProp

instance
  ( ResolveTo res ret
  ) =>
  Mapping ResolveToProp res ret where
  mapping ResolveToProp _ = unsafeCoerce unit

resolveProps :: forall res ret. HMap ResolveToProp res ret => res -> ret
resolveProps = hmap ResolveToProp

-- TESTS 

testRes :: forall res ret. ResolveTo res ret => Proxy res -> Proxy ret
testRes _ = Proxy

-- x :: Proxy Int
-- intTest :: Proxy Int
-- intTest = testRes (Proxy :: Proxy (GqlIo Aff Int))

-- recordTest :: Proxy
--   { a :: Boolean
--   , x :: Int
--   , y :: { z :: String
--          }
--   }
recordTest :: Proxy Int
recordTest = testRes
  ( Proxy
      :: Proxy
           ( Int
             -> GqlObj "test"
                  { x :: Aff Int
                  , y :: (GqlObj "" { z :: Aff String })
                  , a :: Aff Boolean
                  }
           )
  )

fnTest :: Proxy Int
fnTest = testRes (Proxy :: Proxy (Int -> String))