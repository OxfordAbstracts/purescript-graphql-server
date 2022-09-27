module Test.GraphQL.Server.Resolver.ResolverTo where



import Effect.Aff (Aff)
import GraphQL.Resolver.Resolver.GqlObject (GqlObj)
import GraphQL.Resolver.Resolver.ResolveTo (class ResolveTo)
import Type.Proxy (Proxy(..))

-- TYPE LEVEL TESTS 

testRes :: forall res ret. ResolveTo res ret => Proxy res -> Proxy ret
testRes _ = Proxy


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
