module Cuintet.Unit.Csr (
  CsrAddr (..),
  CsrReq (..),
  TrapSpec (..),
  CsrFile (led),
  initCsrFile,
  csrStep,
  csrAccess,
) where

import Clash.Prelude
import Cuintet.Eei (Addr, CsrAddr (..), CsrOp (..), CsrSpec (..), CsrSrc (..), TrapCause (..), XLen)
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

data TrapSpec = TrapSpec
  { epc :: Addr
  , value :: BitVector XLen
  , cause :: TrapCause
  }
  deriving (Generic, NFDataX)

data CsrReq
  = TrapEnter TrapSpec
  | TrapReturn
  deriving (Generic, NFDataX)

csrWrite :: CsrOp -> BitVector XLen -> Maybe (BitVector XLen) -> BitVector XLen
csrWrite ReadWrite oldValue newValueM = fromMaybe oldValue newValueM
csrWrite ReadSet oldValue newValueM = maybe oldValue (oldValue .|.) newValueM
csrWrite ReadClear oldValue newValueM = maybe oldValue ((oldValue .&.) . complement) newValueM

-- | One clock of the CSR file: entering or leaving a trap, and where to fetch from next.
csrStep :: CsrFile -> Maybe CsrReq -> (CsrFile, Maybe Addr)
csrStep file = maybe (ticked, Nothing) (fmap Just . serve ticked)
  where
    ticked = file {mcycle = file.mcycle + 1}

aligned :: BitVector XLen -> BitVector XLen
aligned bits = slice d63 d2 bits ++# zeroBits

serve :: CsrFile -> CsrReq -> (CsrFile, Addr)
serve file (TrapEnter TrapSpec {..}) =
  ( file {mepc = aligned (pack epc), mcause = cause, mtval = value}
  , unpack file.mtvec
  )
serve file TrapReturn = (file, unpack file.mepc)

-- | A CSR instruction: the file it leaves and the value it reads.
csrAccess :: CsrFile -> CsrSpec -> BitVector XLen -> (CsrFile, BitVector XLen)
csrAccess file CsrSpec {..} rs1Data
  | MTVEC <- csrAddr = (file {mtvec = aligned (written file.mtvec)}, file.mtvec)
  | MEPC <- csrAddr = (file {mepc = aligned (written file.mepc)}, file.mepc)
  | MCAUSE <- csrAddr =
      let old = pack file.mcause.interrupt ++# zeroExtend file.mcause.code
       in (file {mcause = trapCause (written old)}, old)
  | MTVAL <- csrAddr = (file {mtval = written file.mtval}, file.mtval)
  | LED <- csrAddr = (file {led = written file.led}, file.led)
  | MCYCLE <- csrAddr = (file {mcycle = written file.mcycle}, file.mcycle)
  | MSTATUS <- csrAddr = (file, 0)
  | MIE <- csrAddr = (file, 0)
  | MHARTID <- csrAddr = (file, 0)
  where
    written old = csrWrite csrOp old (operand <$> csrSrc)
    operand Rs1 = rs1Data
    operand (Uimm v) = zeroExtend v
    trapCause value = TrapCause {interrupt = bitToBool (msb value), code = truncateB value}

initCsrFile :: CsrFile
initCsrFile = CsrFile {mtvec = 0, mepc = 0, mcause = TrapCause False 0, mtval = 0, led = 0, mcycle = 0}
