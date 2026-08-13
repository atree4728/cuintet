{- | MA: the memory access, the CSR access, and the redirect that resolves control flow.

Unlike ID and EX this stage is not a pure function, because the responses of
'loadStoreStep' and 'csrStep' feed back into its own decisions within the same
clock: @stall@ decides whether the instruction commits, and the CSR response
supplies both the value to write back and the redirect. Splitting the unit calls
out of the stage would split the stage itself in two.

@csrFile@ is architectural state and MA is its only writer, which is why a system
instruction can raise its request on the clock it commits without @csrFile@ ever
being updated twice. 'LoadStoreState' is not architectural; it is where a
multi-cycle bus access has got to. They share one record only because MA owns
both.
-}
module Cuintet.Stage.MemAccess (initMemAccessState, memAccess, MemAccessIn (..), MemAccessOut (..), MemAccessState (..)) where

import Clash.Prelude
import Cuintet.CoreCtrl (InstCtrl (..), isBranchOp)
import Cuintet.Eei (Addr, MemReq, MemResp, SystemOp (..))
import Cuintet.Pipeline (ExMa (..), MaWb (..))
import Cuintet.Unit.Csr (CsrAccess (..), CsrAddr (..), CsrFile, CsrReq (..), CsrResp (..), CsrTrap (..), csrStep, initCsrFile, pattern ENVIRONMENT_CALL)
import Cuintet.Unit.LoadStore (InstInfo (..), LoadStoreReq (..), LoadStoreResp (..), LoadStoreState (..), loadStoreStep)
import Cuintet.Util (orNothing)
import Data.Maybe (fromMaybe, isJust)

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
  {- ^ Where IF must restart. Raised only on the clock the instruction commits,
  so a stalled access does not rewrite the PC every clock.
  -}
  , dReq :: Maybe MemReq
  -- ^ Load\/store request, driven from 'LoadStoreState' and never from @dResp@.
  , btbWrite :: Maybe (Addr, Maybe Addr)
  }

{- | One clock of MA. The instruction may sit here for several of them; it leaves
on the one where the access is done with it.
-}
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

    -- the instruction leaves once the access has let go of it
    commit = valid && not loadStoreResp.stall

    -- what to write back; whether and where is WB's decision
    wbData'
      | ctrl.isLoad = fromMaybe (deepErrorX "memAccess: load committed without data") loadStoreResp.rdata
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

    redirect = orNothing (commit && actualNextPc /= predictedNext) actualNextPc

    btbWrite = do
      target <- redirect
      pure (pc, orNothing (target /= pc + 4) target)
