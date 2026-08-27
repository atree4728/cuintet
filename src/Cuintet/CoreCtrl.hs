module Cuintet.CoreCtrl (
  InstFormat (..),
  InstCtrl (..),
  formatCode,
  isMemOp,
  isLoad,
  isStore,
  isBranchOp,
  isCsrRead,
  usesRs1,
  usesRs2,
) where

import Clash.Prelude
import Cuintet.Eei (AluOp, BranchOp, MemOp (..), MulDivOp, SystemOp (..))
import Data.Maybe (isJust)

-- | RISC-V instruction format.
data InstFormat
  = RType
  | IType
  | SType
  | BType
  | UType
  | JType
  deriving (Generic, NFDataX, Eq)

formatCode :: InstFormat -> BitVector 6
formatCode RType = 0b000001
formatCode IType = 0b000010
formatCode SType = 0b000100
formatCode BType = 0b001000
formatCode UType = 0b010000
formatCode JType = 0b100000

-- | Control flags of instruction.
data InstCtrl = InstCtrl
  { format :: InstFormat
  -- ^ Instruction format.
  , rwbEn :: Bool
  -- ^ Whether to enable to write back.
  , isLui :: Bool
  -- ^ Whether to be LUI instruction.
  , aluOp :: Maybe AluOp
  -- ^ The operation the ALU performs; 'Nothing' for the instructions that only need it to add.
  , isOp32 :: Bool
  -- ^ Whether to be either OP_REG_32 or OP_IMM_32.
  , isJump :: Bool
  -- ^ Whether to be jump instruction.
  , memOp :: Maybe MemOp
  -- ^ What the instruction asks of memory; 'Nothing' unless @LOAD@ or @STORE@.
  , branchOp :: Maybe BranchOp
  -- ^ The condition to test; 'Nothing' unless @BRANCH@.
  , mulDivOp :: Maybe MulDivOp
  , systemOp :: Maybe SystemOp
  -- ^ What the instruction asks of the execution environment; 'Nothing' unless @SYSTEM@.
  }
  deriving (Generic, NFDataX)

isMemOp :: InstCtrl -> Bool
isMemOp InstCtrl {memOp} = isJust memOp

isLoad :: InstCtrl -> Bool
isLoad InstCtrl {memOp = Just (Load _ _)} = True
isLoad _ = False

isStore :: InstCtrl -> Bool
isStore InstCtrl {memOp = Just (Store _)} = True
isStore _ = False

isBranchOp :: InstCtrl -> Bool
isBranchOp InstCtrl {branchOp} = isJust branchOp

isCsrRead :: InstCtrl -> Bool
isCsrRead InstCtrl {systemOp = Just (SysCsr _)} = True
isCsrRead _ = False

-- | Whether the instruction form actually reads that source register.
usesRs1, usesRs2 :: InstCtrl -> Bool
usesRs1 InstCtrl {format} = case format of
  UType -> False
  JType -> False
  _ -> True
usesRs2 InstCtrl {format} = case format of
  RType -> True
  SType -> True
  BType -> True
  _ -> False
