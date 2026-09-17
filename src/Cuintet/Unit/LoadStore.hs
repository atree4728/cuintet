-- | Load\/store unit: owns the data bus, on which the committed store at the head of the store queue goes before a load.
module Cuintet.Unit.LoadStore (LoadJob (..), LoadState (..), LoadStoreReq (..), LoadStoreResp (..), loadStoreStep) where

import Clash.Prelude
import Cuintet.Completion (Completion (..))
import Cuintet.Eei (Addr, BusReq (..), BusResp (..), LoadShape, MemDataBytes, MemReq, MemResp, PRegAddr, RobAddr, StoreQueueAddr, loadResult)
import Cuintet.Forwarding (Forwarding)
import Cuintet.Forwarding qualified as F
import Cuintet.Unit.Rob (RobDone (..))
import Cuintet.Unit.StoreQueue (Forward (..), StoreQueueResp, loadForward, storeReq)
import Data.Maybe (isJust, isNothing)

-- | One load for the unit to carry out.
data LoadJob = LoadJob
  { addr :: Addr
  , shape :: LoadShape
  , sqAddr :: StoreQueueAddr
  , pdAddr :: Maybe PRegAddr
  , robAddr :: RobAddr
  , mispredicted :: Bool
  }
  deriving (Generic, NFDataX)

-- | The load the unit holds; a store needs no state here, since the store queue keeps it until memory takes it.
data LoadState
  = Idle
  | WaitReady LoadJob
  | WaitValid LoadJob Forward
  | Waiting Completion
  deriving (Generic, NFDataX)

data LoadStoreReq = LoadStoreReq
  { job :: Maybe LoadJob
  , sq :: StoreQueueResp
  , memResp :: MemResp
  , granted :: Bool
  , squash :: Bool
  }

data LoadStoreResp = LoadStoreResp
  { busy :: Bool
  , done :: Maybe Completion
  , forwarding :: Forwarding
  , memReq :: Maybe MemReq
  , written :: Bool
  }

loadStoreStep :: LoadState -> LoadStoreReq -> (LoadState, LoadStoreResp)
loadStoreStep state LoadStoreReq {..} = (state', LoadStoreResp {..})
  where
    -- The store path: the committed store at the head writes, and leaves the queue once memory has taken it.
    store = storeReq sq
    written = isJust store && memResp.ready

    -- The load path: the bus is the load's for as long as no store writes.
    (state', loadReq, done)
      | squash = (Idle, Nothing, Nothing)
      | otherwise = loadStep state job sq memResp {ready = memResp.ready && isNothing store} granted

    memReq = store <|> loadReq
    forwarding = maybe F.Idle F.broadcast done
    busy = not squash && case state of
      Idle -> False
      _ -> True

-- | One clock of the load path, on the bus as the load sees it: a store writing takes the readiness away.
loadStep :: LoadState -> Maybe LoadJob -> StoreQueueResp -> MemResp -> Bool -> (LoadState, Maybe MemReq, Maybe Completion)
loadStep Idle job _ _ _ = (maybe Idle WaitReady job, Nothing, Nothing)
loadStep state@(WaitReady job) _ sq memResp _ =
  ( if memResp.ready then WaitValid job (loadForward sq job.sqAddr job.addr job.shape) else state
  , Just BusReq {addr = job.addr, wdata = Nothing}
  , Nothing
  )
loadStep state@(WaitValid job fwd) _ _ memResp granted = case (memResp.rdata, fwd) of
  (Nothing, _) -> (state, Nothing, Nothing)
  (Just _, Stall) -> (WaitReady job, Nothing, Nothing)
  (Just w, NoMatch) -> settle granted (completion job w)
  (Just _, Forwarded w) -> settle granted (completion job w)
loadStep (Waiting c) _ _ _ granted = settle granted c

settle :: Bool -> Completion -> (LoadState, Maybe MemReq, Maybe Completion)
settle granted c = (if granted then Idle else Waiting c, Nothing, Just c)

completion :: LoadJob -> BitVector (MemDataBytes * 8) -> Completion
completion LoadJob {..} word =
  Complete robAddr pdAddr RobDone {exception = Nothing, mispredicted, value = loadResult shape word, mem = Just BusReq {addr, wdata = Nothing}}
