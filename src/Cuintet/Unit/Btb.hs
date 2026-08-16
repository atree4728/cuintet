module Cuintet.Unit.Btb (BtbReq (..), BtbResp (..), BtbWrite (..), Prediction (..), btb, predicted, train) where

import Clash.Prelude
import Control.Monad (guard)
import Cuintet.Eei (Addr)
import Cuintet.Util (orNothing)
import Data.Maybe (fromMaybe, isJust)

type IdxBits = 8

type TagBits = 16

type TargetBits = 30

type Hint = Index 4

data Prediction = Prediction {target :: Addr, hint :: Hint}
  deriving (Generic, NFDataX)

data BtbEntry = BtbEntry
  { tag :: BitVector TagBits
  , target :: BitVector TargetBits
  , hint :: Hint
  }
  deriving (Generic, NFDataX, Show, Eq)

mkBtbEntry :: Addr -> Addr -> Hint -> BtbEntry
mkBtbEntry pc target hint = BtbEntry {tag = tagOf pc, target = packTarget target, hint}

data BtbWrite = BtbWrite
  { pc :: Addr
  , target :: Addr
  , hint :: Hint
  }
  deriving (Generic, NFDataX)

data BtbReq = BtbReq
  { lookupAddr :: Addr
  , prefetchAddr :: Addr
  , write :: Maybe BtbWrite
  }
  deriving (Generic, NFDataX)

newtype BtbResp = BtbResp {prediction :: Maybe Prediction}
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
    -- the blockRam output is undefined for the first clock out of reset
    armed = register False (pure True)
    entry =
      blockRamPow2
        (replicate (SNat @(2 ^ IdxBits)) Nothing)
        (idxOf . (.prefetchAddr) <$> req)
        (toWrite . (.write) <$> req)

    toWrite w = do
      BtbWrite {..} <- w
      pure (idxOf pc, Just (mkBtbEntry pc target hint))

    lookupEntry ready pc e
      | not ready = Nothing
      | otherwise = do
          BtbEntry {..} <- e
          guard $ tag == tagOf pc
          pure $ Prediction {target = unpackTarget pc target, hint}

-- | Where a prediction says the instruction at @pc@ goes next.
predicted :: Addr -> Maybe Prediction -> Addr
predicted pc prediction = fromMaybe (pc + 4) $ do
  Prediction {target, hint} <- prediction
  orNothing (hint >= 2) target

train :: Addr -> Maybe Prediction -> Maybe Addr -> Maybe BtbWrite
train pc prediction taken = case prediction of
  Just Prediction {target, hint} -> Just BtbWrite {pc, target = fromMaybe target taken, hint = bump hint}
  Nothing -> (\target -> BtbWrite {pc, target, hint = 2}) <$> taken -- initially weakly taken
  where
    bump = (if isJust taken then satSucc else satPred) SatBound
