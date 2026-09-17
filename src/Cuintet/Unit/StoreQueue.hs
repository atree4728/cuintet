module Cuintet.Unit.StoreQueue (StoreQueueEntry (..), StoreQueueReq (..), StoreQueueResp (..), Forward (..), storeQueue, storeReq, loadForward, overlaps) where

import Clash.Prelude
import Control.Monad (guard)
import Cuintet.Eei (Addr, BusReq (..), CommitWidth, DispatchWidth, LoadShape (..), MemDataBytes, MemReq, NStoreQueue, StoreLanes (..), StoreQueueAddr, laneMask)
import Data.Function (applyWhen)
import Data.Maybe (fromMaybe, isJust)

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

data Forward = NoMatch | Forwarded (BitVector (MemDataBytes * 8)) | Stall | OrderFail
  deriving (Generic, NFDataX)

-- | What the store queue has for a load whose older stores are @[hd, sqAddr)@: the youngest of them that writes a byte it reads.
-- A store whose address is still unknown is taken to write elsewhere. One that covers only part of the load is waited for
-- when committed; otherwise waiting in the unit could block an older load, so the load is to run again from the ROB head.
loadForward :: StoreQueueResp -> StoreQueueAddr -> Addr -> LoadShape -> Forward
loadForward StoreQueueResp {entries, hd, cm} sqAddr addr LoadShape {width, offset} = case fold later hits of
  Nothing -> NoMatch
  Just (age', bytes)
    | and (zipWith (\m b -> not m || isJust b) mask bytes) -> Forwarded (bitCoerce (reverse (fromMaybe 0 <$> bytes)))
    | age' < age cm -> Stall
    | otherwise -> OrderFail
  where
    mask = laneMask width offset
    age i = i - hd
    older i = age i < age sqAddr

    hits = imap hit entries
    hit i e = do
      let a = numConvert i
      guard (older a)
      StoreQueueEntry {addr = storeAddr, lanes = StoreLanes bytes} <- e
      guard (overlaps addr mask storeAddr (isJust <$> bytes))
      pure (age a, bytes)

    later l r = case (l, r) of
      (Just (x, _), Just (y, _)) | y > x -> r
      (Nothing, _) -> r
      _ -> l

-- | Whether two accesses share a byte: the same bus word, and a lane both cover.
overlaps :: Addr -> Vec MemDataBytes Bool -> Addr -> Vec MemDataBytes Bool -> Bool
overlaps a m b n = word a == word b && or (zipWith (&&) m n)
  where
    word x = x `shiftR` natToNum @(CLog 2 MemDataBytes)
