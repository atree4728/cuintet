module Cuintet.Forwarding (NBypasses, bypass, memForward) where

import Clash.Prelude
import Control.Monad (guard)
import Cuintet.Eei (Addr, MemDataBytes, StoreLanes (..), StoreQueueAddr)
import Cuintet.Unit.StoreQueue (StoreQueueEntry (..), StoreQueueResp (..))

type NBypasses = 6

bypass :: (Eq k) => Vec n (Maybe (k, v)) -> k -> v -> v
bypass writes k stored = foldl pick stored writes
  where
    pick _ (Just (k', v)) | k' == k = v
    pick acc _ = acc

-- | Store-to-load forwarding
memForward :: StoreQueueResp -> StoreQueueAddr -> Addr -> StoreLanes MemDataBytes
memForward StoreQueueResp {entries, hd} sqAddr addr = StoreLanes (byte <$> indicesI)
  where
    wordAddr x = x `shiftR` natToNum @(CLog 2 MemDataBytes)
    older a = a - hd < sqAddr - hd

    stores = imap hits entries
    hits i e = do
      StoreQueueEntry {addr = storeAddr, lanes = StoreLanes bytes} <- e
      guard (older (numConvert i) && wordAddr storeAddr == wordAddr addr)
      pure bytes

    byte k = youngest ((>>= (!! k)) <$> below) <|> youngest ((>>= (!! k)) <$> stores)
    below = imap (\i s -> guard (numConvert i < sqAddr) *> s) stores
    youngest = fold (flip (<|>))
