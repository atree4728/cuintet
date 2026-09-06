-- | IF: runs ahead of the rest on its own, filling the IF-ID buffer.
module Cuintet.Stage.Fetch (FetchState (..), initFetchState, FetchIn (..), FetchOut (..), fetch) where

import Clash.Prelude
import Cuintet.Eei (Addr, BusReq (..), BusResp (..), InstsPerBusWord, MemReq, MemResp, NLanes, instSlice, resetVector)
import Cuintet.Pipeline (IfId (..), IfIdDepth)
import Cuintet.Unit.Btb (BtbResp (..), Prediction (..), bankOf)
import Cuintet.Unit.Ring (RingResp (..))
import Cuintet.Upto (Upto (..))
import Cuintet.Upto qualified as Upto
import Cuintet.Util (orNothing)
import Data.Maybe (fromMaybe, isJust)

-- | A fetch in flight: the address, and what the BTB said about it at the time.
data Fetching = Fetching
  { pc :: Addr
  , predictions :: Vec InstsPerBusWord (Maybe Prediction)
  -- ^ Kept until the instruction arrives, since MA needs it to train the BTB.
  }
  deriving (Generic, NFDataX)

-- | The registers IF owns, one per arrow of the fetch path.
data FetchState = FetchState
  { next :: Addr
  -- ^ The address to fetch next; the predicted successor of @fetching@, if any.
  , fetching :: Maybe Fetching
  -- ^ The fetch whose response has not come back yet.
  , staged :: Upto InstsPerBusWord IfId
  -- ^ Fetched instructions waiting for room in the IF-ID buffer.
  }
  deriving (Generic, NFDataX)

-- | Execution starts at address 0 with nothing in flight.
initFetchState :: FetchState
initFetchState =
  FetchState
    { next = resetVector
    , fetching = Nothing
    , staged = Upto.none
    }

data FetchIn = FetchIn
  { iResp :: MemResp
  -- ^ Response to a fetch request issued on an earlier clock.
  , buf :: RingResp IfIdDepth NLanes IfId
  -- ^ The IF-ID buffer, for the room it has.
  , redirect :: Maybe Addr
  -- ^ Where to restart, once MA has resolved control flow.
  , btbResp :: BtbResp
  }

data FetchOut = FetchOut
  { issue :: Upto InstsPerBusWord IfId
  -- ^ What the IF-ID buffer takes this clock.
  , iReq :: Maybe MemReq
  -- ^ Instruction fetch request.
  , btbLookup :: Addr
  , btbPrefetch :: Addr
  }

-- | One clock of IF.
fetch :: FetchState -> FetchIn -> (FetchState, FetchOut)
fetch FetchState {..} FetchIn {..} =
  ( FetchState {next = next', fetching = fetching', staged = staged'}
  , FetchOut {issue, iReq, btbLookup = next, btbPrefetch = next'}
  )
  where
    room = buf.free >= numConvert staged.len + natToNum @InstsPerBusWord
    iReq = orNothing room BusReq {addr = next, wdata = Nothing}
    accepted = room && iResp.ready

    (next', fetching')
      | Just target <- redirect = (target, Nothing)
      | accepted = (fromMaybe fallthrough firstTaken, Just Fetching {pc = next, predictions = aligned})
      | otherwise = (next, fetching)
      where
        aligned
          | bankOf next == 0 = btbResp.predictions
          | otherwise = (btbResp.predictions !! (1 :: Index 2)) :> Nothing :> Nil
        firstTaken = fold (<|>) (takenTarget <$> aligned)
        fallthrough = (next .&. complement 7) + 8

    fetched = mkGroup <$> fetching <*> iResp.rdata
    mkGroup Fetching {..} busWord =
      Upto {len = min insts.len cut, elems = izipWith entry insts.elems predictions}
      where
        insts = instSlice pc busWord
        cut = maybe maxBound (\i -> numConvert i + 1) (findIndex (isJust . takenTarget) predictions)
        entry i instBits prediction = IfId {pc = pc + 4 * numConvert i, instBits, prediction}

    takenTarget p = do
      Prediction {target, hint} <- p
      orNothing (hint >= 2) target

    pushed = buf.free >= numConvert staged.len
    issue = if pushed then staged else Upto.none

    staged'
      | isJust redirect = Upto.none
      | Just entry <- fetched = entry
      | pushed = Upto.none
      | otherwise = staged
{-# OPAQUE fetch #-}
