module Cuintet.Unit.Btb (BtbReq (..), BtbResp (..), btb) where

import Clash.Prelude
import Cuintet.Eei (Addr)
import Cuintet.Util (orNothing)

type IdxBits = 8

type TagBits = 16

type TargetBits = 30

data BtbEntry = BtbEntry
  { tag :: BitVector TagBits
  , target :: BitVector TargetBits
  }
  deriving (Generic, NFDataX, Show, Eq)

data BtbReq = BtbReq
  { lookupAddr :: Addr
  , prefetchAddr :: Addr
  , write :: Maybe (Addr, Maybe Addr)
  }
  deriving (Generic, NFDataX)

newtype BtbResp = BtbResp {predicted :: Maybe Addr}
  deriving newtype (Generic, NFDataX)

idxOf :: Addr -> Unsigned IdxBits
idxOf pc = unpack (slice d9 d2 (pack pc))

tagOf :: Addr -> BitVector TagBits
tagOf pc = slice d25 d10 (pack pc)

packTarget :: Addr -> BitVector TargetBits
packTarget addr = slice d31 d2 (pack addr)

unpackTarget :: Addr -> BitVector TargetBits -> Addr
unpackTarget pc t = unpack (slice d63 d32 (pack pc) ++# t ++# (0 :: BitVector 2))

btb :: (HiddenClockResetEnable dom) => Signal dom BtbReq -> Signal dom BtbResp
btb req = BtbResp <$> (lookupEntry <$> armed <*> ((.lookupAddr) <$> req) <*> entry)
  where
    armed = register False (pure True)
    entry =
      blockRamPow2
        (replicate (SNat @(2 ^ IdxBits)) Nothing)
        (idxOf . (.prefetchAddr) <$> req)
        (toWrite . (.write) <$> req)

    toWrite w = do
      (pc, target) <- w
      pure (idxOf pc, mkEntry pc <$> target)
    mkEntry pc target = BtbEntry {tag = tagOf pc, target = packTarget target}

    lookupEntry ready pc e
      | not ready = Nothing
      | otherwise = do
          BtbEntry {tag, target} <- e
          orNothing (tag == tagOf pc) (unpackTarget pc target)
