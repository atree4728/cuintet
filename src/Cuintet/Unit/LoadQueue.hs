-- | The load queue: every load from rename to commit, with the bytes it reads once it executes.
module Cuintet.Unit.LoadQueue (LoadQueueEntry (..), LoadQueueReq (..), LoadQueueResp (..), loadQueue) where

import Clash.Prelude
import Cuintet.Eei (Addr, CommitWidth, DispatchWidth, LoadQueueAddr, MemDataBytes, NLoadQueue, StoreLanes (..))
import Cuintet.Unit.StoreQueue (StoreQueueEntry (..), overlaps)
import Data.Maybe (isJust)

data LoadQueueEntry = LoadQueueEntry {addr :: Addr, mask :: Vec MemDataBytes Bool}
  deriving (Generic, NFDataX)

data LoadQueueReq = LoadQueueReq
  { allocates :: Index (DispatchWidth + 1)
  , record :: Maybe (LoadQueueAddr, LoadQueueEntry)
  , store :: Maybe (LoadQueueAddr, StoreQueueEntry)
  -- ^ An executing store, with the first load younger than it.
  , failed :: Maybe LoadQueueAddr
  -- ^ A load that met a store it can neither forward from nor wait for.
  , pops :: Index (CommitWidth + 1)
  , squash :: Bool
  }

data LoadQueueResp = LoadQueueResp
  { tl :: LoadQueueAddr
  , orderFail :: Vec CommitWidth Bool
  -- ^ From the head on.
  }

data LoadQueueState = LoadQueueState
  { entries :: Vec NLoadQueue (Maybe LoadQueueEntry)
  -- ^ 'Nothing' until the load executes.
  , orderFail :: Vec NLoadQueue Bool
  , hd :: LoadQueueAddr
  , tl :: LoadQueueAddr
  }
  deriving (Generic, NFDataX)

loadQueue :: (HiddenClockResetEnable dom) => Signal dom LoadQueueReq -> Signal dom LoadQueueResp
loadQueue = mealy (\s req -> (step s req, resp s)) LoadQueueState {entries = repeat Nothing, orderFail = repeat False, hd = 0, tl = 0}
  where
    resp LoadQueueState {orderFail, hd, tl} = LoadQueueResp {tl, orderFail = (\k -> orderFail !! (hd + numConvert k)) <$> indicesI @CommitWidth}

step :: LoadQueueState -> LoadQueueReq -> LoadQueueState
step LoadQueueState {..} LoadQueueReq {..} = LoadQueueState {entries = entries', orderFail = orderFail', hd = hd', tl = tl'}
  where
    hd' = hd + numConvert pops
    tl'
      | squash = hd'
      | otherwise = tl + numConvert allocates
    allocated i = not squash && i - tl < numConvert allocates

    violates i entry = case (store, entry) of
      (Just (first, StoreQueueEntry {addr, lanes = StoreLanes bytes}), Just LoadQueueEntry {addr = loadAddr, mask}) ->
        i - first < tl - first && overlaps loadAddr mask addr (isJust <$> bytes)
      _ -> False

    recorded = maybe entries (\(a, e) -> replace a (Just e) entries) record
    entries' = imap (\i e -> if allocated (numConvert i) then Nothing else e) recorded
    orderFail' = izipWith (\i f e -> let a = numConvert i in not (allocated a) && (f || violates a e || failed == Just a)) orderFail entries
