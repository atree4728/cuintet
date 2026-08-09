module Cuintet.Stage.MemAccess (initMemAccessState, memAccess, MemAccessIn (..), MemAccessOut (..), MemAccessState (..)) where

import Clash.Prelude
import Cuintet.CoreCtrl (InstCtrl (..), isBranchOp)
import Cuintet.CsrUnit (CsrAccess (..), CsrAddr (..), CsrFile, CsrReq (..), CsrResp (..), CsrTrap (..), csrStep, initCsrFile, pattern ENVIRONMENT_CALL)
import Cuintet.Eei (Addr, MemReq, MemResp, SystemOp (..))
import Cuintet.LoadStoreUnit (InstInfo (..), LoadStoreReq (..), LoadStoreResp (..), LoadStoreState (..), loadStoreStep)
import Cuintet.Pipeline (ExMa (..), MaWb (..))
import Cuintet.Util (orNothing)
import Data.Maybe (fromMaybe, isJust)

data MemAccessState = MemAccessState
  { csrFile :: CsrFile
  , loadStoreState :: LoadStoreState
  }
  deriving (Generic, NFDataX)

initMemAccessState :: MemAccessState
initMemAccessState =
  MemAccessState
    { csrFile = initCsrFile
    , loadStoreState = Idle
    }

data MemAccessIn = MemAccessIn
  { entry :: Maybe ExMa
  , dResp :: MemResp
  }

data MemAccessOut = MemAccessOut
  { issue :: Maybe MaWb
  , redirect :: Maybe Addr
  , dReq :: Maybe MemReq
  }

memAccess :: MemAccessState -> MemAccessIn -> (MemAccessState, MemAccessOut)
memAccess MemAccessState {..} MemAccessIn {..} =
  ( MemAccessState {csrFile = csrFile', loadStoreState = loadStoreState'}
  , MemAccessOut {issue = orNothing commit maWb, redirect, dReq = loadStoreResp.memReq}
  )
  where
    valid = isJust entry
    ExMa {..} = fromMaybe (deepErrorX "memAccess: EX-MA FIFO is empty") entry

    (csrFile', csrResp) = maybe (csrFile, Nothing) (fmap Just . csrStep csrFile) csrReq
    csrReq
      | not valid = Nothing
      | Just (SysCsr csrOp) <- ctrl.systemOp =
          Just $ Access CsrAccess {csrAddr = CsrAddr (slice d11 d0 imm), csrOp, rs1Addr, rs1Data}
      | Just SysEcall <- ctrl.systemOp = Just $ Trap CsrTrap {pc, mcause = ENVIRONMENT_CALL}
      | Just SysMret <- ctrl.systemOp = Just Mret
      | otherwise = Nothing
    csrRdata = case csrResp of Just (Accessed v) -> Just v; _ -> Nothing
    csrRedirect = case csrResp of Just (Redirect a) -> Just a; _ -> Nothing

    (loadStoreState', loadStoreResp) =
      loadStoreStep
        loadStoreState
        LoadStoreReq
          { inst = orNothing valid InstInfo {ctrl, addr = bitCoerce aluResult, wdata = rs2Data}
          , memResp = dResp
          }

    commit = valid && not loadStoreResp.stall

    wbData'
      | ctrl.isLoad = fromMaybe (deepErrorX "coreT: load committed without data") loadStoreResp.rdata
      | Just rdata <- csrRdata = rdata
      | otherwise = wbData

    maWb =
      MaWb
        { branchTaken = orNothing (isBranchOp ctrl) branchTaken
        , wbData = wbData'
        , csrRdata
        , ..
        }

    redirect
      | not commit = Nothing
      | isJust csrRedirect = csrRedirect
      | ctrl.isJump = Just (bitCoerce (aluResult .&. complement 1))
      | isBranchOp ctrl && branchTaken = Just (pc + numConvert imm)
      | otherwise = Nothing
