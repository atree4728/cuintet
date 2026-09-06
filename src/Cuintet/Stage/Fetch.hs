-- | IF: runs ahead of the rest on its own, filling the IF-ID buffer.
module Cuintet.Stage.Fetch (FetchState (..), initFetchState, FetchIn (..), FetchOut (..), fetch) where

import Clash.Prelude
import Cuintet.Eei (Addr, BusReq (..), BusResp (..), MemReq, MemResp, NLanes, instAt, resetVector)
import Cuintet.Pipeline (IfId (..), IfIdDepth)
import Cuintet.Unit.Btb (BtbResp (..), Prediction (..), bankOf, predicted)
import Cuintet.Unit.Ring (RingResp (..))
import Cuintet.Upto (Upto (..))
import Cuintet.Upto qualified as Upto
import Cuintet.Util (orNothing)
import Data.Maybe (isJust)

-- | A fetch in flight: the address, and what the BTB said about it at the time.
data Fetching = Fetching
  { pc :: Addr
  , prediction :: Maybe Prediction
  -- ^ Kept until the instruction arrives, since MA needs it to train the BTB.
  }
  deriving (Generic, NFDataX)

-- | The registers IF owns, one per arrow of the fetch path.
data FetchState = FetchState
  { next :: Addr
  -- ^ The address to fetch next; the predicted successor of @fetching@, if any.
  , fetching :: Maybe Fetching
  -- ^ The fetch whose response has not come back yet.
  , staged :: Upto 1 IfId
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
  { issue :: Upto 1 IfId
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
    -- A fetch is only started with room for both @staged@ and the instruction it brings back.
    room = buf.free >= 2
    iReq = orNothing room BusReq {addr = next, wdata = Nothing}
    accepted = room && iResp.ready

    (next', fetching')
      | Just target <- redirect = (target, Nothing)
      | accepted = (predicted next prediction, Just Fetching {pc = next, prediction})
      | otherwise = (next, fetching)
      where
        prediction = btbResp.predictions !! bankOf next

    fetched = mkGroup <$> fetching <*> iResp.rdata
    mkGroup Fetching {..} busWord = Upto {len = 1, elems = IfId {pc, instBits = instAt pc busWord, prediction} :> Nil}

    -- Whatever is offered is taken, so @issue@ is exactly what the buffer writes.
    pushed = buf.free >= 1
    issue = if pushed then staged else Upto.none

    staged'
      | isJust redirect = Upto.none
      | Just entry <- fetched = entry
      | pushed = Upto.none
      | otherwise = staged
{-# OPAQUE fetch #-}
