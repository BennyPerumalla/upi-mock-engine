-- |
-- Module      : UPIMock.Infrastructure.ReadModel
-- Description : The CQRS read side: an STM projection of the event log.
--
-- The read model is derived state. It is never the system of record, it is never
-- migrated, and it is never repaired — it is rebuilt from the log at boot
-- ("UPIMock.Application.Service"'s @rebuildReadModel@). That is what licenses
-- keeping it in memory in Phase 1, and it is why every operation here is total.
--
-- __Why one 'TVar' and not two.__ The projection needs a primary index by
-- 'UPIMock.Domain.Types.TxnId' and a secondary index by
-- 'UPIMock.Domain.Types.Rrn'. Holding them in separate 'TVar's would be equally
-- correct as long as every operation stayed inside a single 'atomically', but it
-- would make that a convention a reviewer has to check. One 'TVar' over a record
-- makes a torn read unrepresentable: a reader takes one snapshot and both indices
-- in it agree by construction. The cost is that unrelated writes conflict and
-- retry, which for a projection updated once per committed transaction is not a
-- cost worth optimising in Phase 1.
--
-- __Phase 2.__ This module is where the chaos harness will grow a lag knob —
-- delay or drop 'putViewIO' to reproduce the read-after-write anomaly that
-- eventual consistency actually produces in a PSP. The interface does not change
-- to allow that; only this module does.
module UPIMock.Infrastructure.ReadModel
  ( -- * Handle
    ReadModel
  , newReadModel

    -- * Operations
  , putViewIO
  , getViewIO
  , getViewByRrnIO
  , queryViewsIO
  , replaceAllViewsIO
  , readModelCount
  ) where

import Control.Concurrent.STM
  ( TVar
  , atomically
  , modifyTVar'
  , newTVarIO
  , readTVarIO
  , writeTVar
  )
import Data.List (foldl', sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..))

import UPIMock.Application.Ports (ViewQuery (..))
import UPIMock.Domain.Transaction (TxnView (..))
import UPIMock.Domain.Types (Rrn, TxnId)

-- | A consistent snapshot of both indices.
--
-- @projByRrn@ maps to a 'TxnId' rather than to a 'TxnView' so that a view exists
-- exactly once in memory and the two indices cannot disagree about its contents.
data Projection = Projection
  { projByTxn :: Map TxnId TxnView
  , projByRrn :: Map Rrn TxnId
  }

newtype ReadModel = ReadModel (TVar Projection)

emptyProjection :: Projection
emptyProjection = Projection {projByTxn = Map.empty, projByRrn = Map.empty}

newReadModel :: IO ReadModel
newReadModel = ReadModel <$> newTVarIO emptyProjection

-- | Upsert, honouring the monotonicity law from
-- "UPIMock.Application.Ports": a write whose
-- 'UPIMock.Domain.Transaction.viewVersion' does not exceed the stored one is
-- dropped.
--
-- This is not defensive coding. Two commits on the same stream serialise in the
-- store but their subsequent projections do not, so the older one can arrive
-- second; without this check the read model would sit on the stale state until
-- something else happened to that transaction, which for a terminal transaction
-- is forever.
putViewIO :: ReadModel -> TxnView -> IO ()
putViewIO (ReadModel var) view = atomically (modifyTVar' var (insertView view))

insertView :: TxnView -> Projection -> Projection
insertView view projection
  | Just existing <- Map.lookup key (projByTxn projection)
  , viewVersion existing >= viewVersion view =
      projection
  | otherwise =
      Projection
        { projByTxn = Map.insert key view (projByTxn projection)
        , projByRrn = Map.insert (viewRrn view) key (projByRrn projection)
        }
  where
    key = viewTxnId view

-- | Point reads take a snapshot and never retry: 'readTVarIO' is a single read,
-- so there is no transaction to conflict with a concurrent 'putViewIO'.
getViewIO :: ReadModel -> TxnId -> IO (Maybe TxnView)
getViewIO (ReadModel var) txnId = Map.lookup txnId . projByTxn <$> readTVarIO var

-- | Two lookups against /one/ snapshot. A missing view for a present RRN would
-- mean the indices had diverged, which the single-'TVar' design makes impossible;
-- 'Nothing' therefore means \"no such RRN\" and nothing else.
getViewByRrnIO :: ReadModel -> Rrn -> IO (Maybe TxnView)
getViewByRrnIO (ReadModel var) rrn = do
  projection <- readTVarIO var
  pure $ do
    txnId <- Map.lookup rrn (projByRrn projection)
    Map.lookup txnId (projByTxn projection)

-- | Filter, order, paginate — in memory, and unashamedly so: this is a simulator
-- whose working set is one developer's test scenarios, and a query planner here
-- would be machinery in place of a @filter@.
--
-- The ordering is most-recently-updated first, broken by descending version and
-- then by 'TxnId'. The tiebreaks are not decoration: without a total order, two
-- views sharing a timestamp can swap places between requests and a paginating
-- client will see a row twice or not at all.
--
-- A negative limit or offset is clamped rather than rejected, because the port is
-- total and the HTTP layer has already had its chance to reject.
queryViewsIO :: ReadModel -> ViewQuery -> IO [TxnView]
queryViewsIO (ReadModel var) q = do
  projection <- readTVarIO var
  pure
    . take (max 0 (queryLimit q))
    . drop (max 0 (queryOffset q))
    . sortOn ordering
    . filter matches
    . Map.elems
    $ projByTxn projection
  where
    ordering view = (Down (viewUpdatedAt view), Down (viewVersion view), viewTxnId view)
    matches view =
      maybe True (== viewState view) (queryState q)
        && maybe True (== viewFlow view) (queryFlow q)

-- | Replace the projection wholesale. Boot-time rebuild only.
--
-- The fold reuses 'insertView', so a log that somehow produced two views for one
-- stream resolves the same way it would at runtime — highest version wins —
-- rather than by list order.
replaceAllViewsIO :: ReadModel -> [TxnView] -> IO ()
replaceAllViewsIO (ReadModel var) views =
  atomically (writeTVar var (foldl' (flip insertView) emptyProjection views))

-- | Number of transactions in the projection. For the health endpoint and the
-- boot log; not a business metric.
readModelCount :: ReadModel -> IO Int
readModelCount (ReadModel var) = Map.size . projByTxn <$> readTVarIO var
