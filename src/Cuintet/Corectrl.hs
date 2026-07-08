module Cuintet.Corectrl where

import Clash.Prelude

-- | RISC-V instruction type
data InstType
  = RType
  | IType
  | SType
  | BType
  | UType
  | JType
  deriving (Generic, NFDataX, Eq)

instCode :: InstType -> BitVector 6
instCode RType = 0b000001
instCode IType = 0b000010
instCode SType = 0b000100
instCode BType = 0b001000
instCode UType = 0b010000
instCode JType = 0b100000

-- | Control flags of instruction.
data InstCtrl = InstCtrl
  { itype :: InstType
  -- ^ Instruction type.
  , rwbEn :: Bool
  -- ^ Whether to enable to write back.
  , isLui :: Bool
  -- ^ Whether to be LUI instruction.
  , isAluOp :: Bool
  -- ^ Whether to use the ALU.
  , isJump :: Bool
  -- ^ Whether to be jump instruction.
  , isLoad :: Bool
  -- ^ Whether to be load instruction.
  , funct3 :: BitVector 3
  -- ^ @funct3@ field.
  , funct7 :: BitVector 7
  -- ^ @funct7@ field.
  }
  deriving (Generic, NFDataX)
