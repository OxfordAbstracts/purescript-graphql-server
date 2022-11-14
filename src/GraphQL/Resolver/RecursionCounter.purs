module GraphQL.Resolver.RecursionCounter where

import Prelude

import Type.Data.Peano (Nat)
import Type.Proxy (Proxy)




class NameCount :: Type -> Nat -> Constraint
class NameCount l n where 
  nameCount :: l -> Proxy n