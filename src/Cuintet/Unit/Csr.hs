module Cuintet.Unit.Csr (
  CsrAddr (..),
  CsrReq (..),
  CsrAccess (..),
  CsrTrap (..),
  CsrResp (..),
  CsrFile (led),
  initCsrFile,
  csrStep,
) where

import Clash.Prelude
import Cuintet.Eei (Addr, CsrOp (..), CsrSrc (..), TrapCause (..), XLen)
import Cuintet.Util (orNothing)
import Data.Maybe (fromMaybe)

newtype CsrAddr = CsrAddr (BitVector 12)
  deriving newtype (BitPack, Generic, NFDataX)

pattern MTVEC, MEPC, MCAUSE, MTVAL, LED, MCYCLE :: CsrAddr
pattern MTVEC = CsrAddr 0x305
pattern MEPC = CsrAddr 0x341
pattern MCAUSE = CsrAddr 0x342
pattern MTVAL = CsrAddr 0x343
pattern LED = CsrAddr 0x800
pattern MCYCLE = CsrAddr 0xB00

-- | @mcause@ as it reads: the interrupt flag in the top bit, the code in the bottom.
mcauseValue :: TrapCause -> BitVector XLen
mcauseValue TrapCause {interrupt, code} = pack interrupt ++# zeroExtend code

-- | The inverse. The bits between the flag and the code name no cause, and are dropped.
mcauseCause :: BitVector XLen -> TrapCause
mcauseCause value = TrapCause {interrupt = bitToBool (msb value), code = truncateB value}

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

data CsrAccess = CsrAccess
  { csrAddr :: CsrAddr
  , op :: CsrOp
  , src :: CsrSrc
  , rs1Addr :: BitVector 5
  , rs1Data :: BitVector XLen
  }
  deriving (Generic, NFDataX)

-- | Enter a trap taken at @epc@. What raised it is the caller's business.
data CsrTrap = CsrTrap
  { epc :: Addr
  , value :: BitVector XLen
  , cause :: TrapCause
  }
  deriving (Generic, NFDataX)

-- | The two things the unit is asked for are mutually exclusive.
data CsrReq
  = Access CsrAccess
  | Trap CsrTrap
  | Mret
  deriving (Generic, NFDataX)

data CsrResp
  = Accessed (BitVector XLen)
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
serve file (Trap CsrTrap {..}) =
  ( file {mepc = aligned (pack epc), mcause = cause, mtval = value}
  , Redirect $ unpack file.mtvec
  )
serve file Mret = (file, Redirect $ unpack file.mepc)
serve file (Access CsrAccess {..})
  | MTVEC <- csrAddr =
      let old = unpack file.mtvec
       in (file {mtvec = aligned (written old)}, Accessed old)
  | MEPC <- csrAddr =
      let old = unpack file.mepc
       in (file {mepc = aligned (written old)}, Accessed old)
  | MCAUSE <- csrAddr =
      let old = mcauseValue file.mcause
       in (file {mcause = mcauseCause (written old)}, Accessed old)
  | MTVAL <- csrAddr =
      let old = unpack file.mtval
       in (file {mtval = written old}, Accessed old)
  | LED <- csrAddr = (file {led = written file.led}, Accessed file.led)
  | MCYCLE <- csrAddr = (file {mcycle = written file.mcycle}, Accessed file.mcycle)
  | otherwise = deepErrorX "csrStep: unimplemented CSR instruction"
  where
    written old = csrWrite op old wdata
    wvalue = case src of
      FromRs1 -> rs1Data
      FromUimm -> zeroExtend rs1Addr
    wdata = case op of
      ReadWrite -> Just wvalue
      -- For both CSRRS and CSRRC, if rs1=x0, then the instruction will not write to the CSR at all
      _ -> orNothing (rs1Addr /= 0) wvalue

initCsrFile :: CsrFile
initCsrFile = CsrFile {mtvec = 0, mepc = 0, mcause = TrapCause False 0, mtval = 0, led = 0, mcycle = 0}
