{- |
The payloads that cross the stage boundaries, one record per FIFO.

Each stage passes on everything it was given and appends what it computed, so
the records grow down the pipe and 'MaWb' carries the whole history of an
instruction. That is what makes it usable as the core's execution log, which
'Cuintet.Debug.showInstLog' prints. Nothing is dropped and only @wbData@ is ever
rewritten, by MA for a load or a CSR read.
-}
module Cuintet.Pipeline (IfId (..), IdEx (..), ExMa (..), MaWb (..), srcRegs, destReg, forwardable) where

import Clash.Prelude
import Cuintet.CoreCtrl (InstCtrl (..))
import Cuintet.Eei (Addr, Inst, RegAddr, XLen)
import Cuintet.Util (orNothing)
import GHC.Records (HasField)

-- | What IF fetched: the instruction word and the address it came from.
data IfId = IfId
  { pc :: Addr
  , instBits :: Inst
  }
  deriving (Generic, NFDataX)

-- | 'IfId' plus the decoded control, the immediate, and the register operands.
data IdEx = IdEx
  { pc :: Addr
  , instBits :: Inst
  , ctrl :: InstCtrl
  , imm :: BitVector XLen
  , rs1Addr :: RegAddr
  , rs2Addr :: RegAddr
  , rdAddr :: RegAddr
  , rs1Data :: BitVector XLen
  , rs2Data :: BitVector XLen
  }
  deriving (Generic, NFDataX)

-- | 'IdEx' plus what the ALU and the branch unit produced.
data ExMa = ExMa
  { pc :: Addr
  , instBits :: Inst
  , ctrl :: InstCtrl
  , imm :: BitVector XLen
  , rs1Addr :: RegAddr
  , rs2Addr :: RegAddr
  , rdAddr :: RegAddr
  , rs1Data :: BitVector XLen
  , rs2Data :: BitVector XLen
  , op1 :: BitVector XLen
  , op2 :: BitVector XLen
  , aluResult :: BitVector XLen
  -- ^ Also the access address of a load\/store and the target of a jump.
  , branchTaken :: Bool
  -- ^ Meaningful only for a B-type instruction.
  , wbData :: BitVector XLen
  -- ^ The value to write back as far as EX can tell.
  }
  deriving (Generic, NFDataX)

-- | 'ExMa' plus what the memory and CSR accesses produced.
data MaWb = MaWb
  { pc :: Addr
  , instBits :: Inst
  , ctrl :: InstCtrl
  , imm :: BitVector XLen
  , rs1Addr :: RegAddr
  , rs2Addr :: RegAddr
  , rdAddr :: RegAddr
  , rs1Data :: BitVector XLen
  , rs2Data :: BitVector XLen
  , op1 :: BitVector XLen
  , op2 :: BitVector XLen
  , aluResult :: BitVector XLen
  , branchTaken :: Maybe Bool
  -- ^ 'Nothing' unless the instruction is a branch.
  , wbData :: BitVector XLen
  -- ^ The loaded word, the old CSR value, or what EX computed.
  , csrRdata :: Maybe (BitVector XLen)
  -- ^ The old CSR value, for a CSR access.
  }
  deriving (Generic, NFDataX)

{- | The @rs1@ and @rs2@ fields, shared by ID and the register file read.

The read is addressed from the instruction word before it is decoded, so this
has to be readable off the raw bits.
-}
srcRegs :: Inst -> (RegAddr, RegAddr)
srcRegs instBits = (slice d19 d15 instBits, slice d24 d20 instBits)

{- | The register this instruction writes, if any: the single definition of it,
shared by the interlock in ID and the write in WB.

It is stated over the fields rather than a payload type so that it applies at
every stage the instruction passes through.
-}
destReg :: (HasField "ctrl" stage InstCtrl, HasField "rdAddr" stage RegAddr) => stage -> Maybe RegAddr
destReg stage = orNothing (stage.ctrl.rwbEn && stage.rdAddr /= 0) stage.rdAddr

forwardable ::
  (HasField "ctrl" stage InstCtrl, HasField "rdAddr" stage RegAddr, HasField "wbData" stage t) =>
  stage -> Maybe (RegAddr, t)
forwardable stage = (,stage.wbData) <$> destReg stage
