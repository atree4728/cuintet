module Cuintet.Unit.Btb (BtbReq (..), BtbResp (..), BtbWrite (..), Prediction (..), btb, predicted, train, bankOf, isTaken) where

import Clash.Prelude
import Control.Monad (guard)
import Cuintet.Eei (Addr, FetchWidth)
import Cuintet.Util (orNothing)
import Data.Maybe (fromMaybe, isJust)

type Idx = Unsigned 7

type Tag = BitVector 16

-- | A target with the bits the PC already carries dropped: @target[31:2]@.
type PackedTarget = BitVector 30

data Hint = StronglyNotTaken | WeaklyNotTaken | WeaklyTaken | StronglyTaken
  deriving (Generic, NFDataX, Show, Eq)

isTaken :: Hint -> Bool
isTaken = \case
  WeaklyTaken -> True
  StronglyTaken -> True
  _ -> False

bump :: Bool -> Hint -> Hint
bump True = \case
  StronglyNotTaken -> WeaklyNotTaken
  WeaklyNotTaken -> WeaklyTaken
  _ -> StronglyTaken
bump False = \case
  StronglyTaken -> WeaklyTaken
  WeaklyTaken -> WeaklyNotTaken
  _ -> StronglyNotTaken

data Prediction = Prediction {target :: Addr, hint :: Hint}
  deriving (Generic, NFDataX)

data BtbEntry = BtbEntry
  { tag :: Tag
  , target :: PackedTarget
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

newtype BtbResp = BtbResp {predictions :: Vec FetchWidth (Maybe Prediction)}
  deriving newtype (Generic, NFDataX)

idxOf :: Addr -> Idx
idxOf pc = unpack (slice d9 d3 (pack pc))

bankOf :: Addr -> Index FetchWidth
bankOf pc = unpack (slice d2 d2 (pack pc))

tagOf :: Addr -> Tag
tagOf pc = slice d25 d10 (pack pc)

packTarget :: Addr -> PackedTarget
packTarget addr = slice d31 d2 (pack addr)

unpackTarget :: Addr -> PackedTarget -> Addr
unpackTarget pc t = unpack (slice d63 d32 (pack pc) ++# t ++# (0 :: BitVector 2))

btb :: (HiddenClockResetEnable dom) => Signal dom BtbReq -> Signal dom BtbResp
btb req = BtbResp <$> (lookupPair <$> armed <*> ((.lookupAddr) <$> req) <*> bundle entries)
  where
    -- the blockRam output is undefined for the first clock out of reset
    armed = register False (pure True)
    entries = bank <$> (indicesI @FetchWidth)
    bank i =
      blockRamPow2
        (repeat Nothing)
        (idxOf . (.prefetchAddr) <$> req)
        (toWrite i . (.write) <$> req)

    toWrite i w = do
      BtbWrite {..} <- w
      guard (bankOf pc == i)
      pure (idxOf pc, Just (mkBtbEntry pc target hint))

    lookupPair ready base = imap (\i e -> hit ready (base .&. complement 0b111 + 4 * numConvert i) e)
    hit ready pc e
      | not ready = Nothing
      | otherwise = do
          BtbEntry {..} <- e
          guard $ tag == tagOf pc
          pure $ Prediction {target = unpackTarget pc target, hint}

-- | Where a prediction says the instruction at @pc@ goes next.
predicted :: Addr -> Maybe Prediction -> Addr
predicted pc prediction = fromMaybe (pc + 4) $ do
  Prediction {target, hint} <- prediction
  orNothing (isTaken hint) target

train :: Addr -> Maybe Prediction -> Maybe Addr -> Maybe BtbWrite
train pc prediction taken = case prediction of
  Just Prediction {target, hint} -> Just BtbWrite {pc, target = fromMaybe target taken, hint = bump (isJust taken) hint}
  Nothing -> (\target -> BtbWrite {pc, target, hint = WeaklyTaken}) <$> taken
