-- | Load unit: reads the data read bus, and takes from the store queue what the older stores have not yet written.
module Cuintet.Unit.Load (LoadJob (..), LoadState (..), LoadReq (..), LoadResp (..), loadStep, holder) where

import Clash.Prelude
import Cuintet.Completion (Completion (..), regWrite, robWrite, trapped)
import Cuintet.Eei (Addr, BusReadReq (..), BusReadResp (..), LoadQueueAddr, LoadShape, MemAccess (..), MemDataBytes, MemReadResp, PRegAddr, RobAddr, StoreLanes (..), StoreQueueAddr, TrapCause, XLen, loadResult)
import Cuintet.Unit.Rob (RobDone (..))
import Cuintet.Unit.StoreQueue (StoreQueueResp, memForward)
import Data.Maybe (fromMaybe)

-- | One load for the unit to carry out.
data LoadJob = LoadJob
  { addr :: Addr
  , shape :: LoadShape
  , sqAddr :: StoreQueueAddr
  , lqAddr :: LoadQueueAddr
  , pdAddr :: Maybe PRegAddr
  , robAddr :: RobAddr
  , mispredicted :: Bool
  , exception :: Maybe (TrapCause, BitVector XLen)
  }
  deriving (Generic, NFDataX)

data LoadState
  = Idle
  | WaitReady LoadJob
  | WaitValid LoadJob (StoreLanes MemDataBytes)
  | Waiting Completion
  deriving (Generic, NFDataX)

data LoadReq = LoadReq
  { job :: Maybe LoadJob
  , sq :: StoreQueueResp
  , dReadResp :: MemReadResp
  , granted :: Bool
  , squash :: Bool
  }

data LoadResp = LoadResp
  { busy :: Bool
  , done :: Maybe Completion
  , bypass :: Maybe (PRegAddr, BitVector XLen)
  , dReadReq :: Maybe BusReadReq
  }

loadStep :: LoadState -> LoadReq -> (LoadState, LoadResp)
loadStep state LoadReq {..} = (state', LoadResp {bypass = regWrite =<< done, ..})
  where
    (state', dReadReq, done)
      | squash = (Idle, Nothing, Nothing)
      | otherwise = case state of
          Idle -> (maybe Idle start job, Nothing, Nothing)
          WaitReady j ->
            ( if dReadResp.ready then WaitValid j (memForward sq j.sqAddr j.addr) else state
            , Just BusReadReq {addr = j.addr}
            , Nothing
            )
          WaitValid j forwarded -> case dReadResp.rdata of
            Nothing -> (state, Nothing, Nothing)
            Just w -> settle (completion j (overlay forwarded w))
          Waiting c -> settle c

    start j = maybe (WaitReady j) (Waiting . trapped j.robAddr) j.exception

    settle c = (if granted then Idle else Waiting c, Nothing, Just c)

    busy =
      not squash && case state of
        Idle -> False
        _ -> True

-- | The word read, with the bytes the older stores have not yet written laid over it.
overlay :: StoreLanes MemDataBytes -> BitVector (MemDataBytes * 8) -> BitVector (MemDataBytes * 8)
overlay (StoreLanes forwarded) w = bitCoerce (zipWith fromMaybe (bitCoerce w) (reverse forwarded))

completion :: LoadJob -> BitVector (MemDataBytes * 8) -> Completion
completion LoadJob {..} word =
  Completion robAddr pdAddr RobDone {exception = Nothing, mispredicted, value = loadResult shape word, mem = Just (LoadAccess addr)}

-- | The entry the unit holds, for the trace.
holder :: LoadState -> Maybe RobAddr
holder = \case
  WaitReady job -> Just job.robAddr
  WaitValid job _ -> Just job.robAddr
  Waiting c -> Just (fst (robWrite c))
  Idle -> Nothing
