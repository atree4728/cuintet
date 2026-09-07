module Cuintet.CoreCtrl (
  InstFormat (..),
  InstCtrl (..),
  isLoad,
  isCsrRead,
  usesRs1,
  usesRs2,
  isJalr,
) where

import Clash.Prelude
import Cuintet.Eei (AluOp, BranchOp, MemOp (..), MulDivOp, SystemOp (..))

-- | RISC-V instruction format.
data InstFormat
  = RType
  | IType
  | SType
  | BType
  | UType
  | JType
  deriving (Generic, NFDataX, Eq)

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

isLoad :: InstCtrl -> Bool
isLoad InstCtrl {memOp = Just (Load _ _)} = True
isLoad _ = False

isCsrRead :: InstCtrl -> Bool
isCsrRead InstCtrl {systemOp = Just (SysCsr _)} = True
isCsrRead _ = False

isJalr :: InstCtrl -> Bool
isJalr InstCtrl {isJump, format} = isJump && format == IType

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
