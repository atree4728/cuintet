module Cuintet.Forwarding (NBypasses, bypass) where

import Clash.Prelude

type NBypasses = 6

bypass :: (Eq k) => Vec n (Maybe (k, v)) -> k -> v -> v
bypass writes k stored = foldl pick stored writes
  where
    pick _ (Just (k', v)) | k' == k = v
    pick acc _ = acc
