-- | MA: the memory access, the CSR access, and the redirect that resolves control flow.
module Cuintet.Stage.MemAccess (initMemAccessState, memAccess, MemAccessIn (..), MemAccessOut (..), MemAccessState (..)) where

import Clash.Prelude
import Control.Monad (guard)
import Cuintet.CoreCtrl (InstCtrl (..), isBranchOp, isLoad)
import Cuintet.Eei (Addr, MemReq, MemResp, SystemOp (..))
import Cuintet.Pipeline (ExMa (..), MaWb (..))
import Cuintet.Unit.Btb (BtbWrite, predicted, train)
import Cuintet.Unit.Csr (AccessSpec (..), CsrAddr (..), CsrFile, CsrReq (..), CsrResp (..), TrapSpec (..), csrStep, initCsrFile)
import Cuintet.Unit.LoadStore (LoadStoreJob (..), LoadStoreReq (..), LoadStoreResp (..), LoadStoreState (..), loadStoreStep)
import Cuintet.Util (orNothing)
import Data.Maybe (fromMaybe, isJust, isNothing)

-- | The registers MA owns.
data MemAccessState = MemAccessState
  { csrFile :: CsrFile
  -- ^ Architectural state; MA is its only writer.
  , loadStoreState :: LoadStoreState
  -- ^ Where the multi-cycle bus access has got to.
  }
  deriving (Generic, NFDataX)

-- | A cleared CSR file and no access in flight.
initMemAccessState :: MemAccessState
initMemAccessState =
  MemAccessState
    { csrFile = initCsrFile
    , loadStoreState = Idle
    }

data MemAccessIn = MemAccessIn
  { entry :: Maybe ExMa
  -- ^ The instruction at the head of the EX-MA FIFO.
  , dResp :: MemResp
  -- ^ Response to a load\/store request issued on an earlier clock.
  }

data MemAccessOut = MemAccessOut
  { issue :: Maybe MaWb
  -- ^ The instruction handed to WB, present only on the clock it commits.
  , redirect :: Maybe Addr
  -- ^ Where IF must restart.
  , dReq :: Maybe MemReq
  -- ^ Load\/store request, driven from 'LoadStoreState' and never from @dResp@.
  , btbWrite :: Maybe BtbWrite
  -- ^ What the resolved instruction teaches the BTB, on the clock it commits.
  }

-- | One clock of MA.
memAccess :: MemAccessState -> MemAccessIn -> (MemAccessState, MemAccessOut)
memAccess MemAccessState {..} MemAccessIn {..} =
  ( MemAccessState {csrFile = csrFile', loadStoreState = loadStoreState'}
  , MemAccessOut {issue = orNothing commit maWb, redirect, dReq = loadStoreResp.memReq, btbWrite}
  )
  where
    valid = isJust entry
    ExMa {..} = fromMaybe (deepErrorX "memAccess: EX-MA FIFO is empty") entry

    (csrFile', csrResp) = csrStep csrFile csrReq
    csrReq
      | not valid = Nothing
      | Just (cause, value) <- exception = Just $ TrapEnter TrapSpec {epc = pc, ..}
      | Just (SysCsr (src, op)) <- ctrl.systemOp =
          Just $ CsrAccess AccessSpec {csrAddr = CsrAddr (slice d11 d0 imm), op, src, rs1Addr, rs1Data}
      | Just SysMret <- ctrl.systemOp = Just TrapReturn
      | otherwise = Nothing
    csrRdata = case csrResp of Just (ReadValue v) -> Just v; _ -> Nothing
    csrRedirect = case csrResp of Just (Redirect a) -> Just a; _ -> Nothing

    (loadStoreState', loadStoreResp) =
      loadStoreStep
        loadStoreState
        LoadStoreReq
          { job = orNothing (valid && isNothing exception) LoadStoreJob {ctrl, addr = bitCoerce aluResult, wdata = rs2Data}
          , memResp = dResp
          }

    -- the instruction leaves once the access has let go of it
    commit = valid && not loadStoreResp.stall

    -- what to write back; whether and where is WB's decision
    wbData'
      | isLoad ctrl = fromMaybe (deepErrorX "memAccess: load committed without data") loadStoreResp.result
      | Just rdata <- csrRdata = rdata
      | otherwise = wbData

    maWb =
      MaWb
        { branchTaken = orNothing (isBranchOp ctrl) branchTaken
        , wbData = wbData'
        , csrRdata
        , ..
        }

    actualNextPc
      | Just target <- csrRedirect = target
      | ctrl.isJump = bitCoerce (aluResult .&. complement 1)
      | isBranchOp ctrl && branchTaken = pc + numConvert imm
      | otherwise = pc + 4

    redirect = orNothing (commit && actualNextPc /= predicted pc prediction) actualNextPc

    taken = orNothing (actualNextPc /= pc + 4) actualNextPc

    btbWrite = guard commit >> train pc prediction taken
{-# OPAQUE memAccess #-}
