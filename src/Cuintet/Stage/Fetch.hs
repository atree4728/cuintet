{- |
IF: runs ahead of the rest on its own, filling the IF-ID FIFO.

Each of the three arrows out of a register (@next@ to the bus, the bus to
@staged@, @staged@ to the FIFO) takes a clock, so an instruction reaches the head
of the FIFO three clocks after its request goes out, while the throughput stays
at one instruction per clock. The FIFO absorbs the difference between that and
the rate ID drains it at.

@iReq@ must not depend combinationally on @iResp@, or a combinational loop closes
through 'Cuintet.BusArbiter.busArbiter'. It is driven from @next@ and the FIFO's
@wreadyTwo@, both register outputs. A fetch goes out only when the FIFO has room
for the staged write and this one both.
-}
module Cuintet.Stage.Fetch (FetchState (..), initFetchState, FetchIn (..), FetchOut (..), fetch) where

import Clash.Prelude
import Cuintet.Eei (Addr, BusReq (..), BusResp (..), MemReq, MemResp, instAt)
import Cuintet.Pipeline (IfId (..))
import Cuintet.Unit.BranchPredict (predict)
import Cuintet.Unit.Fifo (FifoResp (..))
import Cuintet.Util (orNothing)
import Data.Maybe (isJust)

-- | The registers IF owns, one per arrow of the fetch path.
data FetchState = FetchState
  { next :: Addr
  -- ^ The address to fetch next.
  , fetching :: Maybe Addr
  -- ^ The address whose response has not come back yet.
  , staged :: Maybe IfId
  -- ^ A fetched instruction waiting for room in the IF-ID FIFO.
  }
  deriving (Generic, NFDataX)

-- | Execution starts at address 0 with nothing in flight.
initFetchState :: FetchState
initFetchState =
  FetchState
    { next = 0
    , fetching = Nothing
    , staged = Nothing
    }

data FetchIn = FetchIn
  { iResp :: MemResp
  -- ^ Response to a fetch request issued on an earlier clock.
  , fifo :: FifoResp IfId
  -- ^ The IF-ID FIFO, for the room it has.
  , redirect :: Maybe Addr
  -- ^ Where to restart, once MA has resolved control flow.
  }

data FetchOut = FetchOut
  { issue :: Maybe IfId
  -- ^ What to write into the IF-ID FIFO.
  , iReq :: Maybe MemReq
  -- ^ Instruction fetch request.
  , predRedirect :: Maybe Addr
  }

{- | One clock of IF. A redirect wins over everything: it drops both the fetch
in flight and the staged instruction, which the flush of the FIFO matches.
-}
fetch :: FetchState -> FetchIn -> (FetchState, FetchOut)
fetch FetchState {..} FetchIn {..} =
  ( FetchState {next = next', fetching = fetching', staged = staged'}
  , FetchOut {issue = staged, iReq, predRedirect}
  )
  where
    iReq = orNothing fifo.wreadyTwo BusReq {addr = next, wdata = Nothing}
    accepted = fifo.wreadyTwo && iResp.ready
    -- the answer to the request issued when @fetching@ was set
    predicted = mkIfId <$> fetching <*> iResp.rdata
    mkIfId pc busWord = IfId {pc = pc, instBits, predictedNext}
      where
        instBits = instAt pc busWord
        predictedNext = predict pc instBits

    predRedirect = do
      entry <- predicted
      orNothing (entry.predictedNext /= entry.pc + 4) entry.predictedNext

    (next', fetching')
      | Just target <- redirect = (target, Nothing)
      | Just target <- predRedirect = (target, Nothing)
      | accepted = (next + 4, Just next)
      | otherwise = (next, fetching)

    staged'
      | isJust redirect = Nothing
      | Just entry <- predicted = Just entry
      | fifo.wready = Nothing -- the staged write was accepted
      | otherwise = staged -- pending
