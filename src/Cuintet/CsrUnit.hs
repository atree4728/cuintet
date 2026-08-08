module Cuintet.CsrUnit (
  CsrAddr (..),
  MCause (..),
  pattern ENVIRONMENT_CALL,
  CsrReq (..),
  CsrAccess (..),
  CsrTrap (..),
  CsrResp (..),
  CsrFile,
  initCsrFile,
  csrStep,
) where

import Clash.Prelude
import Cuintet.Eei (Addr, CsrOp (..), CsrType (..), RegAddr, XLen)
import Cuintet.Util (orNothing)
import Data.Maybe (fromMaybe)

newtype CsrAddr = CsrAddr (BitVector 12)
  deriving newtype (BitPack, Generic, NFDataX)

pattern MTVEC, MEPC, MCAUSE :: CsrAddr
pattern MTVEC = CsrAddr 0x305
pattern MEPC = CsrAddr 0x341
pattern MCAUSE = CsrAddr 0x342

{- | The reason a trap was taken. No exception code in use here goes above 15,
so the code is kept narrow and widened only where @mcause@ is read.
-}
data MCause
  = MCause
  { interrupt :: Bool
  , code :: BitVector 4
  }
  deriving (Generic, NFDataX)

deriveAutoReg ''MCause

pattern ENVIRONMENT_CALL :: MCause
pattern ENVIRONMENT_CALL = MCause False 11

mcauseValue :: MCause -> BitVector XLen
mcauseValue MCause {interrupt, code} = pack interrupt ++# zeroExtend code

{- | Addresses are held without the bits that are always zero: @mtvec@ in Direct
mode and @mepc@ both have a four-byte aligned base.
-}
data CsrFile = CsrFile
  { mtvecBase :: BitVector 62 -- (XLen - 2); see https://github.com/clash-lang/clash-compiler/issues/3100
  , mepcAligned :: BitVector 62 -- (XLen - 2)
  , mcause :: MCause
  }
  deriving (Generic, NFDataX)

deriveAutoReg ''CsrFile

data CsrAccess = CsrAccess
  { csrAddr :: CsrAddr
  , csrOp :: CsrOp
  , rs1Addr :: RegAddr -- uimm in the @CsrImm@ case
  , rs1Data :: BitVector XLen
  }
  deriving (Generic, NFDataX)

-- | Enter a trap taken at @pc@. What raised it is the caller's business.
data CsrTrap = CsrTrap
  { pc :: Addr
  , mcause :: MCause
  }
  deriving (Generic, NFDataX)

-- | The two things the unit is asked for are mutually exclusive.
data CsrReq
  = Access CsrAccess
  | Trap CsrTrap
  | Mret
  deriving (Generic, NFDataX)

data CsrResp
  = -- | for CsrAccess
    Accessed (BitVector XLen)
  | -- | for trap or mret
    Redirect Addr
  deriving (Generic, NFDataX)

csrWrite :: CsrType -> BitVector XLen -> Maybe (BitVector XLen) -> BitVector XLen
csrWrite ReadWrite oldValue newValueM = fromMaybe oldValue newValueM
csrWrite ReadSet oldValue newValueM = maybe oldValue (oldValue .|.) newValueM
csrWrite ReadClear oldValue newValueM = maybe oldValue ((oldValue .&.) . complement) newValueM
csrWrite CSRIllegal _ _ = deepErrorX "csrWrite: illegal System instruction"

csrStep :: CsrFile -> CsrReq -> (CsrFile, CsrResp)
csrStep file (Trap CsrTrap {..}) =
  ( file {mepcAligned = slice d63 d2 (pack pc), mcause}
  , Redirect $ bitCoerce $ file.mtvecBase ++# (0 :: BitVector 2)
  )
csrStep file Mret = (file, Redirect $ bitCoerce $ file.mepcAligned ++# (0 :: BitVector 2))
csrStep file (Access CsrAccess {..})
  | CSRIllegal <- csrType = deepErrorX "csrStep: illegal System instruction"
  | MTVEC <- csrAddr =
      let old = file.mtvecBase ++# (0 :: BitVector 2)
       in (file {mtvecBase = slice d63 d2 (csrWrite csrType old wdata)}, Accessed old)
  | MEPC <- csrAddr =
      let old = file.mepcAligned ++# (0 :: BitVector 2)
       in (file {mepcAligned = slice d63 d2 (csrWrite csrType old wdata)}, Accessed old)
  | MCAUSE <- csrAddr = (file, Accessed (mcauseValue file.mcause))
  | otherwise = deepErrorX "csrStep: unimplemented CSR instruction"
  where
    (wvalue, csrType) = case csrOp of
      CsrReg t -> (rs1Data, t)
      CsrImm t -> (zeroExtend rs1Addr, t)
    wdata = case csrType of
      ReadWrite -> Just wvalue
      -- For both CSRRS and CSRRC, if rs1=x0, then the instruction will not write to the CSR at all
      _ -> orNothing (rs1Addr /= 0) wvalue

initCsrFile :: CsrFile
initCsrFile = CsrFile {mtvecBase = 0, mepcAligned = 0, mcause = MCause False 0}
