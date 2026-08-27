module Cuintet.Unit.Csr (
  CsrAddr (..),
  CsrReq (..),
  AccessSpec (..),
  TrapSpec (..),
  CsrResp (..),
  CsrFile (led),
  initCsrFile,
  csrStep,
) where

import Clash.Prelude
import Cuintet.Eei (Addr, CsrAddr (..), CsrOp (..), CsrSrc (..), TrapCause (..), XLen)
import Cuintet.Util (orNothing)
import Data.Maybe (fromMaybe)

data CsrFile = CsrFile
  { mtvec :: BitVector XLen
  , mepc :: BitVector XLen
  , mcause :: TrapCause
  , mtval :: BitVector XLen
  , led :: BitVector XLen
  , mcycle :: BitVector XLen
  }
  deriving (Generic, NFDataX)

deriveAutoReg ''CsrFile

data AccessSpec = AccessSpec
  { csrAddr :: CsrAddr
  , op :: CsrOp
  , src :: CsrSrc
  , rs1Addr :: BitVector 5
  , rs1Data :: BitVector XLen
  }
  deriving (Generic, NFDataX)

data TrapSpec = TrapSpec
  { epc :: Addr
  , value :: BitVector XLen
  , cause :: TrapCause
  }
  deriving (Generic, NFDataX)

data CsrReq
  = CsrAccess AccessSpec
  | TrapEnter TrapSpec
  | TrapReturn
  deriving (Generic, NFDataX)

data CsrResp
  = ReadValue (BitVector XLen)
  | Redirect Addr
  deriving (Generic, NFDataX)

csrWrite :: CsrOp -> BitVector XLen -> Maybe (BitVector XLen) -> BitVector XLen
csrWrite ReadWrite oldValue newValueM = fromMaybe oldValue newValueM
csrWrite ReadSet oldValue newValueM = maybe oldValue (oldValue .|.) newValueM
csrWrite ReadClear oldValue newValueM = maybe oldValue ((oldValue .&.) . complement) newValueM

-- | One clock of the CSR file.
csrStep :: CsrFile -> Maybe CsrReq -> (CsrFile, Maybe CsrResp)
csrStep file = maybe (ticked, Nothing) (fmap Just . serve ticked)
  where
    ticked = file {mcycle = file.mcycle + 1}

aligned :: BitVector XLen -> BitVector XLen
aligned bits = slice d63 d2 bits ++# zeroBits

serve :: CsrFile -> CsrReq -> (CsrFile, CsrResp)
serve file (TrapEnter TrapSpec {..}) =
  ( file {mepc = aligned (pack epc), mcause = cause, mtval = value}
  , Redirect $ unpack file.mtvec
  )
serve file TrapReturn = (file, Redirect $ unpack file.mepc)
serve file (CsrAccess AccessSpec {..})
  | MTVEC <- csrAddr =
      let old = unpack file.mtvec
       in (file {mtvec = aligned (written old)}, ReadValue old)
  | MEPC <- csrAddr =
      let old = unpack file.mepc
       in (file {mepc = aligned (written old)}, ReadValue old)
  | MCAUSE <- csrAddr =
      let old = pack file.mcause.interrupt ++# zeroExtend file.mcause.code
       in (file {mcause = trapCause (written old)}, ReadValue old)
  | MTVAL <- csrAddr =
      let old = unpack file.mtval
       in (file {mtval = written old}, ReadValue old)
  | LED <- csrAddr = (file {led = written file.led}, ReadValue file.led)
  | MCYCLE <- csrAddr = (file {mcycle = written file.mcycle}, ReadValue file.mcycle)
  where
    written old = csrWrite op old wdata
    wvalue = case src of
      FromRs1 -> rs1Data
      FromUimm -> zeroExtend rs1Addr
    wdata = case op of
      ReadWrite -> Just wvalue
      -- For both CSRRS and CSRRC, if rs1=x0, then the instruction will not write to the CSR at all
      _ -> orNothing (rs1Addr /= 0) wvalue
    trapCause value = TrapCause {interrupt = bitToBool (msb value), code = truncateB value}

initCsrFile :: CsrFile
initCsrFile = CsrFile {mtvec = 0, mepc = 0, mcause = TrapCause False 0, mtval = 0, led = 0, mcycle = 0}
