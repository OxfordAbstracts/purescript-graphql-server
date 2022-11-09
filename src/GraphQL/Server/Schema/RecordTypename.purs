module GraphQL.Server.Schema.RecordTypename where

import Prim.Row (class Lacks)
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RowList
import Record as Record
import Type.Proxy (Proxy(..))

addTypename
  :: forall r169 name
   . Lacks "__typename" r169
  => Record r169
  -> { __typename :: Proxy name
     | r169
     }
addTypename = Record.insert (Proxy :: Proxy "__typename") Proxy

class RecordTypename :: Type -> Symbol -> Constraint
class RecordTypename r name | r -> name, name -> r

instance (RowListTypename rl name, RowToList a rl) => RecordTypename { | a } name

class RowListTypename :: RowList Type -> Symbol -> Constraint
class RowListTypename (a :: RowList Type) name | a -> name

instance RowListTypename (RowList.Cons "__typename" (Proxy typename) tail) typename
else instance RowListTypename tail typename => RowListTypename (RowList.Cons sym t tail) typename

