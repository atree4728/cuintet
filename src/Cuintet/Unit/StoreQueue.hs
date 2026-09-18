module Cuintet.Unit.StoreQueue (StoreQueueReq (..), StoreQueueResp (..), storeQueue, overlaps, memForward) where

import Clash.Prelude
import Control.Monad (guard)
import Cuintet.Eei (Addr, BusWriteReq (..), BusWriteResp (..), CommitWidth, DispatchWidth, MemDataBytes, MemWriteReq, NStoreQueue, StoreLanes (..), StoreQueueAddr)
import Cuintet.Util (isOlder)
import Data.Function (applyWhen)
import Data.Maybe (isJust)

data StoreQueueReq = StoreQueueReq
  { allocates :: Index (DispatchWidth + 1)
  , write :: Maybe (StoreQueueAddr, MemWriteReq)
  , commits :: Index (CommitWidth + 1)
  , dWriteResp :: BusWriteResp
  , squash :: Bool
  }

data StoreQueueResp = StoreQueueResp
  { entries :: Vec NStoreQueue (Maybe MemWriteReq)
  -- ^ 'Nothing' until the store executes.
  , hd :: StoreQueueAddr
  , cm :: StoreQueueAddr
  -- ^ @[hd, cm)@ is committed, @[cm, tl)@ speculative.
  , tl :: StoreQueueAddr
  , free :: StoreQueueAddr
  , storeReq :: Maybe MemWriteReq
  }

data StoreQueueState = StoreQueueState
  { entries :: Vec NStoreQueue (Maybe MemWriteReq)
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
storeReq :: StoreQueueState -> Maybe MemWriteReq
storeReq StoreQueueState {..} = guard (hd /= cm) *> entries !! hd

-- | Whether two accesses share a byte: the same bus word, and a lane both cover.
overlaps :: Addr -> Vec MemDataBytes Bool -> Addr -> Vec MemDataBytes Bool -> Bool
overlaps a m b n = wordAddr a == wordAddr b && or (zipWith (&&) m n)

-- | Store-to-load forwarding
memForward :: StoreQueueResp -> StoreQueueAddr -> Addr -> StoreLanes MemDataBytes
memForward StoreQueueResp {entries, hd} sqAddr addr = StoreLanes (byte <$> indicesI)
  where
    stores = imap hits entries
    hits i e = do
      BusWriteReq {addr = storeAddr, wdata = StoreLanes bytes} <- e
      guard (isOlder (numConvert i) sqAddr hd && wordAddr storeAddr == wordAddr addr)
      pure bytes

    byte k = youngest ((>>= (!! k)) <$> below) <|> youngest ((>>= (!! k)) <$> stores)
    below = imap (\i s -> guard (numConvert i < sqAddr) *> s) stores
    youngest = fold (flip (<|>))

wordAddr :: Addr -> Addr
wordAddr x = x `shiftR` natToNum @(CLog 2 MemDataBytes)
