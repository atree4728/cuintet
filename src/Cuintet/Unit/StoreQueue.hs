module Cuintet.Unit.StoreQueue (StoreQueueEntry (..), StoreQueueReq (..), StoreQueueResp (..), storeQueue, overlaps) where

import Clash.Prelude
import Control.Monad (guard)
import Cuintet.Eei (Addr, BusReq (..), BusResp (..), CommitWidth, DispatchWidth, MemDataBytes, MemReq, MemResp, NStoreQueue, StoreLanes (..), StoreQueueAddr)
import Data.Function (applyWhen)
import Data.Maybe (isJust)

data StoreQueueEntry = StoreQueueEntry
  { addr :: Addr
  , lanes :: StoreLanes MemDataBytes
  }
  deriving (Generic, NFDataX)

data StoreQueueReq = StoreQueueReq
  { allocates :: Index (DispatchWidth + 1)
  , write :: Maybe (StoreQueueAddr, StoreQueueEntry)
  , commits :: Index (CommitWidth + 1)
  , dWriteResp :: MemResp
  , squash :: Bool
  }

data StoreQueueResp = StoreQueueResp
  { entries :: Vec NStoreQueue (Maybe StoreQueueEntry)
  -- ^ 'Nothing' until the store executes.
  , hd :: StoreQueueAddr
  , cm :: StoreQueueAddr
  -- ^ @[hd, cm)@ is committed, @[cm, tl)@ speculative.
  , tl :: StoreQueueAddr
  , free :: StoreQueueAddr
  , storeReq :: Maybe MemReq
  }

data StoreQueueState = StoreQueueState
  { entries :: Vec NStoreQueue (Maybe StoreQueueEntry)
  , hd :: StoreQueueAddr
  , cm :: StoreQueueAddr
  , tl :: StoreQueueAddr
  }
  deriving (Generic, NFDataX)

storeQueue :: (HiddenClockResetEnable dom) => Signal dom StoreQueueReq -> Signal dom StoreQueueResp
storeQueue = mealy (\s req -> (step s req, resp s)) StoreQueueState {entries = repeat Nothing, hd = 0, cm = 0, tl = 0}
  where
    resp s@StoreQueueState {..} =
      StoreQueueResp
        { free = maxBound - (tl - hd)
        , storeReq = storeReq s
        , ..
        }

step :: StoreQueueState -> StoreQueueReq -> StoreQueueState
step s@StoreQueueState {..} StoreQueueReq {..} = StoreQueueState {entries = entries', hd = hd', cm = cm', tl = tl'}
  where
    hd' = applyWhen (isJust (storeReq s) && dWriteResp.ready) (+ 1) hd
    cm' = cm + numConvert commits
    tl'
      | squash = cm'
      | otherwise = tl + numConvert allocates
    allocated i = not squash && i - tl < numConvert allocates
    executed = maybe entries (\(a, e) -> replace a (Just e) entries) (guard (not squash) *> write)
    entries' = imap (\i e -> if allocated (numConvert i) then Nothing else e) executed

-- | The committed store at the head, as the data bus takes it.
storeReq :: StoreQueueState -> Maybe MemReq
storeReq StoreQueueState {..} = do
  guard (hd /= cm)
  StoreQueueEntry {..} <- entries !! hd
  pure BusReq {addr, wdata = Just lanes}

-- | Whether two accesses share a byte: the same bus word, and a lane both cover.
overlaps :: Addr -> Vec MemDataBytes Bool -> Addr -> Vec MemDataBytes Bool -> Bool
overlaps a m b n = word a == word b && or (zipWith (&&) m n)
  where
    word x = x `shiftR` natToNum @(CLog 2 MemDataBytes)
