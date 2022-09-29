module GraphQL.Record.Unsequence where

import Prelude

import Prim.Row as Row
import Prim.RowList as RL
import Record (get) as R
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Prelude (class IsSymbol)
import Type.Proxy (Proxy(..))

class UnsequenceRecord (rl :: RL.RowList Type) row from to m | rl -> row from to m where
  unsequenceRecordImpl :: Proxy rl -> m (Record row) -> (Builder { | from } { | to })

instance unsequenceProxiesNil :: UnsequenceRecord RL.Nil row () () m where
  unsequenceRecordImpl _ _ = identity

instance
  ( IsSymbol name
  , Row.Cons name ty trash row
  , Functor m
  , Row.Lacks name ()
  , Row.Cons name (m ty) () to
  ) =>
  UnsequenceRecord (RL.Cons name ty RL.Nil) row () to m where
  unsequenceRecordImpl _ a =
    Builder.insert namep valA
    where
    namep = Proxy :: _ name

    valA :: m _
    valA = R.get namep <$> a

else instance sequenceRecordCons ::
  ( IsSymbol name
  , Row.Cons name ty trash row
  , Functor m
  , UnsequenceRecord tail row from from' m
  , Row.Lacks name from'
  , Row.Cons name (m ty) from' to
  ) =>
  UnsequenceRecord (RL.Cons name ty tail) row from to m where
  unsequenceRecordImpl _ a =
    Builder.insert namep valA <<< rest
    where
    namep = Proxy :: _ name
    valA = R.get namep <$> a
    tailp = Proxy :: _ tail
    rest = unsequenceRecordImpl tailp a

unsequenceRecord
  :: forall row row' rl m
   . RL.RowToList row rl
  => UnsequenceRecord rl row () row' m
  => m (Record row)
  -> (Record row')
unsequenceRecord a = Builder.build builder {}
  where
  builder = unsequenceRecordImpl (Proxy :: _ rl) a

class UnsequenceProxies :: forall k. k -> Type -> Constraint
class UnsequenceProxies r1 r2 where
  unsequenceProxies :: Proxy r1 -> r2

instance
  ( RL.RowToList row rl
  , UnsequenceRecord rl row () row' Proxy
  ) =>
  UnsequenceProxies (Record row) (Record row') where
  unsequenceProxies = unsequenceRecord