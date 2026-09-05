-- | MA: the memory access, the CSR access, and the redirect that resolves control flow.
module Cuintet.Stage.MemAccess (memAccess, MemAccessIn (..), MemAccessOut (..)) where

import Clash.Prelude
import Cuintet.CoreCtrl (isLoad)
import Cuintet.Eei (MemReq, MemResp)
import Cuintet.Pipeline (ExMa (..), MaCm (..))
import Cuintet.Unit.LoadStore (LoadStoreJob (..), LoadStoreReq (..), LoadStoreResp (..), LoadStoreState (..), loadStoreStep)
import Cuintet.Util (orNothing)
import Data.Maybe (fromMaybe, isJust, isNothing)

data MemAccessIn = MemAccessIn
  { entry :: Maybe ExMa
  -- ^ The instruction at the head of the EX-MA FIFO.
  , dResp :: MemResp
  -- ^ Response to a load\/store request issued on an earlier clock.
  }

data MemAccessOut = MemAccessOut
  { issue :: Maybe MaCm
  -- ^ The instruction handed to WB, present only on the clock it commits.
  , dReq :: Maybe MemReq
  -- ^ Load\/store request, driven from 'LoadStoreState' and never from @dResp@.
  }

-- | One clock of MA.
memAccess :: LoadStoreState -> MemAccessIn -> (LoadStoreState, MemAccessOut)
memAccess loadStoreState MemAccessIn {..} =
  (loadStoreState', MemAccessOut {issue = orNothing commit maWb, dReq = loadStoreResp.memReq})
  where
    valid = isJust entry
    ExMa {..} = fromMaybe (deepErrorX "memAccess: EX-MA FIFO is empty") entry

    (loadStoreState', loadStoreResp) =
      loadStoreStep
        loadStoreState
        LoadStoreReq
          { job = orNothing (valid && isNothing exception) LoadStoreJob {ctrl, addr = bitCoerce aluResult, wdata = rs2Data}
          , memResp = dResp
          }

    commit = valid && not loadStoreResp.stall

    maWb =
      MaCm
        { wbData =
            if isLoad ctrl
              then fromMaybe (deepErrorX "memAccess: load committed without data") loadStoreResp.result
              else wbData
        , completed = loadStoreResp.completed
        , ..
        }
{-# OPAQUE memAccess #-}
