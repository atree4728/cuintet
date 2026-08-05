module Cuintet.CsrUnit (CsrAddr (..), CsrReq (..), CsrResp (..), CsrFile, initCsrFile, csrUnitStep, csrUnit) where

import Clash.Prelude
import Cuintet.Eei (CsrOp (..), CsrType (..), XLen)
import Cuintet.Util (orNothing)
import Data.Maybe (fromMaybe)

newtype CsrAddr = CsrAddr (BitVector 12)
  deriving newtype (BitPack, Generic, NFDataX)

pattern MTVEC :: CsrAddr
pattern MTVEC = CsrAddr 0x305

newtype CsrFile = CsrFile {mtvecBase :: BitVector 30}
  deriving newtype (NFDataX)

data CsrReq = CsrReq
  { csrOp :: CsrOp
  , csrAddr :: CsrAddr
  , rs1Addr :: BitVector 5 -- uimm in the @CsrImm@ case
  , rs1Data :: BitVector XLen
  }
  deriving (Generic, NFDataX)

newtype CsrResp = CsrResp {rdata :: BitVector XLen}
  deriving newtype (NFDataX)

csrWrite :: CsrType -> BitVector XLen -> Maybe (BitVector XLen) -> BitVector XLen
csrWrite ReadWrite oldValue newValueM = fromMaybe oldValue newValueM
csrWrite ReadSet oldValue newValueM = maybe oldValue (oldValue .|.) newValueM
csrWrite ReadClear oldValue newValueM = maybe oldValue ((oldValue .&.) . complement) newValueM
csrWrite CSRIllegal _ _ = deepErrorX "csrWrite: illegal System instruction"

csrUnitStep :: CsrFile -> Maybe CsrReq -> (CsrFile, CsrResp)
csrUnitStep file Nothing = (file, CsrResp {rdata = deepErrorX "csrUnitStep: no request"})
csrUnitStep file (Just (CsrReq {csrOp, csrAddr, rs1Addr, rs1Data}))
  | CSRIllegal <- csrType = deepErrorX "csrUnitStep: illegal System instruction"
  | MTVEC <- csrAddr =
      let old = file.mtvecBase ++# (0 :: BitVector 2)
       in (CsrFile {mtvecBase = slice d31 d2 (csrWrite csrType old wdata)}, CsrResp {rdata = old})
  | otherwise = deepErrorX "csrUnitStep: unimplemented CSR instruction"
  where
    (wvalue, csrType) = case csrOp of
      CsrReg t -> (rs1Data, t)
      CsrImm t -> (zeroExtend rs1Addr, t)
    wdata = case csrType of
      ReadWrite -> Just wvalue
      -- For both CSRRS and CSRRC, if rs1=x0, then the instruction will not write to the CSR at all
      _ -> orNothing (rs1Addr /= 0) wvalue

initCsrFile :: CsrFile
initCsrFile = CsrFile {mtvecBase = 0}

csrUnit :: (HiddenClockResetEnable dom) => Signal dom (Maybe CsrReq) -> Signal dom CsrResp
csrUnit = mealy csrUnitStep initCsrFile
