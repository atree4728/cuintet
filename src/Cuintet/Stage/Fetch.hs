-- | IF: runs ahead of the rest on its own, filling the fetch buffer.
module Cuintet.Stage.Fetch (FetchState (..), initFetchState, FetchIn (..), FetchOut (..), fetch) where

import Clash.Prelude
import Control.Monad (guard)
import Cuintet.Eei (Addr, BusReq (..), BusResp (..), DispatchWidth, FetchWidth, MemReq, MemResp, instSlice, resetVector)
import Cuintet.Pipeline (FetchBufBits, Fetched (..))
import Cuintet.Unit.Btb (BtbResp (..), Prediction (..), bankOf, isTaken)
import Cuintet.Unit.Ring (RingResp (..))
import Cuintet.Util (orNothing)
import Data.Bool (bool)
import Data.Maybe (fromMaybe, isJust, isNothing)

-- | A fetch in flight: the address, and what the BTB said about it at the time.
data Fetching = Fetching
  { pc :: Addr
  , predictions :: Vec FetchWidth (Maybe Prediction)
  -- ^ Kept until the instruction arrives, since MA needs it to train the BTB.
  }
  deriving (Generic, NFDataX)

-- | The registers IF owns, one per arrow of the fetch path.
data FetchState = FetchState
  { next :: Addr
  -- ^ The address to fetch next; the predicted successor of @fetching@, if any.
  , fetching :: Maybe Fetching
  -- ^ The fetch whose response has not come back yet.
  , staged :: Vec FetchWidth (Maybe Fetched)
  -- ^ Fetched instructions waiting for room in the fetch buffer.
  , restart :: Maybe Addr
  -- ^ Where a resolved redirect sends IF, taken one clock later so that it stays out of @next@'s cone.
  }
  deriving (Generic, NFDataX)

-- | Execution starts at address 0 with nothing in flight.
initFetchState :: FetchState
initFetchState =
  FetchState
    { next = resetVector
    , fetching = Nothing
    , staged = repeat Nothing
    , restart = Nothing
    }

data FetchIn = FetchIn
  { iResp :: MemResp
  -- ^ Response to a fetch request issued on an earlier clock.
  , buf :: RingResp FetchBufBits DispatchWidth Fetched
  -- ^ The fetch buffer, for the room it has.
  , redirect :: Maybe Addr
  -- ^ Where to restart, once MA has resolved control flow.
  , btbResp :: BtbResp
  }

data FetchOut = FetchOut
  { issue :: Vec FetchWidth (Maybe Fetched)
  -- ^ What the fetch buffer takes this clock.
  , iReq :: Maybe MemReq
  -- ^ Instruction fetch request.
  , btbLookup :: Addr
  , btbPrefetch :: Addr
  }

-- | One clock of IF.
fetch :: FetchState -> FetchIn -> (FetchState, FetchOut)
fetch FetchState {..} FetchIn {..} =
  ( FetchState {next = next', fetching = fetching', staged = staged', restart = redirect}
  , FetchOut {issue, iReq, btbLookup = next, btbPrefetch = next'}
  )
  where
    nStaged = sum (bool 0 1 . isJust <$> staged)
    room = buf.free >= nStaged + natToNum @FetchWidth
    iReq = orNothing (room && isNothing restart) BusReq {addr = next, wdata = Nothing}
    accepted = isJust iReq && iResp.ready

    next'
      | Just target <- restart = target
      | accepted = fromMaybe fallthrough firstTaken
      | otherwise = next

    fetching'
      | isJust redirect = Nothing
      | accepted = Just Fetching {pc = next, predictions = aligned}
      | otherwise = fetching

    aligned
      | bankOf next == 0 = btbResp.predictions
      | otherwise = (btbResp.predictions !! bankOf next) :> Nothing :> Nil
    firstTaken = fold (<|>) (takenTarget <$> aligned)
    fallthrough = (next .&. complement 7) + 8

    fetched = mkGroup <$> fetching <*> iResp.rdata
    mkGroup Fetching {..} busWord = izipWith entry (instSlice pc busWord) predictions
      where
        cut = findIndex (isJust . takenTarget) predictions
        entry i inst prediction = do
          instBits <- inst
          guard (maybe True (i <=) cut)
          pure Fetched {pc = pc + 4 * numConvert i, instBits, prediction}

    takenTarget p = do
      Prediction {target, hint} <- p
      orNothing (isTaken hint) target

    pushed = buf.free >= nStaged
    issue = if pushed then staged else repeat Nothing

    staged'
      | isJust redirect = repeat Nothing
      | Just entry <- fetched = entry
      | pushed = repeat Nothing
      | otherwise = staged
{-# OPAQUE fetch #-}
