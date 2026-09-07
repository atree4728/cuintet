module Cuintet.Stage.MemAccess (memAccess, MemAccessIn (..), MemAccessOut (..)) where

import Clash.Prelude
import Cuintet.CoreCtrl (InstCtrl (..), isLoad)
import Cuintet.Eei (IssueWidth, MemReq, MemResp, XLen)
import Cuintet.Pipeline (Completed (..), Executed (..))
import Cuintet.Unit.LoadStore (LoadStoreJob (..), LoadStoreReq (..), LoadStoreResp (..), LoadStoreState (..), loadStoreStep)
import Cuintet.Upto (Upto (..))
import Cuintet.Upto qualified as Upto
import Data.Maybe (fromMaybe, isNothing)

data MemAccessIn = MemAccessIn
  { entries :: Upto IssueWidth Executed
  , dResp :: MemResp
  }

data MemAccessOut = MemAccessOut
  { issue :: Upto IssueWidth Completed
  , issued :: Bool
  , dReq :: Maybe MemReq
  }

-- | One clock of MA. The group stalls as a whole; no lane is ever dropped here.
memAccess :: LoadStoreState -> MemAccessIn -> (LoadStoreState, MemAccessOut)
memAccess loadStoreState MemAccessIn {..} = (loadStoreState', MemAccessOut {..})
  where
    (loadStoreState', loadStoreResp) = loadStoreStep loadStoreState LoadStoreReq {job, memResp = dResp}

    job = mkJob =<< Upto.head entries
    mkJob Executed {..}
      | isNothing exception
      , Just memOp <- ctrl.memOp =
          Just LoadStoreJob {memOp, addr = bitCoerce aluResult, wdata = rs2Data}
      | otherwise = Nothing

    issued = entries.len > 0 && not loadStoreResp.stall
    dReq = loadStoreResp.memReq

    issue = Upto {len = if issued then entries.len else 0, elems}
    elems =
      memAccessLane loadStoreResp.result loadStoreResp.completed (entries.elems !! (0 :: Index IssueWidth))
        :> memAccessLane Nothing Nothing (entries.elems !! (1 :: Index IssueWidth))
        :> Nil
{-# OPAQUE memAccess #-}

memAccessLane :: Maybe (BitVector XLen) -> Maybe MemReq -> Executed -> Completed
memAccessLane result completed Executed {..} =
  Completed
    { wbData =
        if isLoad ctrl
          then fromMaybe (deepErrorX "memAccess: load committed without data") result
          else wbData
    , mem = completed
    , ..
    }
