module Cuintet.Unit.StoreQueue (StoreQueueEntry (..), StoreQueueReq (..), StoreQueueResp (..), Forward (..), storeQueue, storeReq, loadForward) where

import Clash.Prelude
import Control.Monad (guard)
import Cuintet.Eei (Addr, BusReq (..), CommitWidth, DispatchWidth, LoadShape, MemDataBytes, MemReq, NStoreQueue, StoreLanes, StoreQueueAddr)
import Data.Function (applyWhen)

data StoreQueueEntry = StoreQueueEntry
  { addr :: Addr
  , lanes :: StoreLanes MemDataBytes
  }
  deriving (Generic, NFDataX)

data StoreQueueReq = StoreQueueReq
  { allocates :: Index (DispatchWidth + 1)
  , write :: Maybe (StoreQueueAddr, StoreQueueEntry)
  , commits :: Index (CommitWidth + 1)
  , written :: Bool
  -- ^ Whether the data bus took the committed store at the head.
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
    resp StoreQueueState {..} = StoreQueueResp {free = maxBound - (tl - hd), ..}

step :: StoreQueueState -> StoreQueueReq -> StoreQueueState
step StoreQueueState {..} StoreQueueReq {..} = StoreQueueState {entries = entries', hd = hd', cm = cm', tl = tl'}
  where
    hd' = applyWhen written (+ 1) hd
    cm' = cm + numConvert commits
    tl'
      | squash = cm'
      | otherwise = tl + numConvert allocates
    allocated i = not squash && i - tl < numConvert allocates
    executed = maybe entries (\(a, e) -> replace a (Just e) entries) (guard (not squash) *> write)
    entries' = imap (\i e -> if allocated (numConvert i) then Nothing else e) executed

-- | The committed store at the head, as the data bus takes it.
storeReq :: StoreQueueResp -> Maybe MemReq
storeReq StoreQueueResp {..} = do
  guard (hd /= cm)
  StoreQueueEntry {..} <- entries !! hd
  pure BusReq {addr, wdata = Just lanes}

data Forward = NoMatch | Forwarded (BitVector (MemDataBytes * 8)) | Stall
  deriving (Generic, NFDataX)

-- | What the store queue has for a load whose older stores are @[hd, sqAddr)@. For now any of them makes it wait.
loadForward :: StoreQueueResp -> StoreQueueAddr -> Addr -> LoadShape -> Forward
loadForward StoreQueueResp {hd} sqAddr _ _
  | sqAddr == hd = NoMatch
  | otherwise = Stall
