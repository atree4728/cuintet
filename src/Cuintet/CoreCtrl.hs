module Cuintet.CoreCtrl (
  InstType (..),
  InstCtrl (..),
  instCode,
  isMemOp,
  isLoad,
  isStore,
  isBranchOp,
  isCsrRead,
  usesRs1,
  usesRs2,
) where

import Clash.Prelude
import Cuintet.Eei (Access (..), AluOp, BranchCond, MulDivType, SystemOp (..))
import Data.Maybe (isJust)

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
  , aluOp :: Maybe AluOp
  -- ^ The operation the ALU performs; 'Nothing' for the instructions that only need it to add.
  , isOp32 :: Bool
  -- ^ Whether to be either OP_REG_32 or OP_IMM_32.
  , isJump :: Bool
  -- ^ Whether to be jump instruction.
  , access :: Maybe Access
  -- ^ What the instruction asks of memory; 'Nothing' unless @LOAD@ or @STORE@.
  , branch :: Maybe BranchCond
  -- ^ The condition to test; 'Nothing' unless @BRANCH@.
  , mulDiv :: Maybe MulDivType
  , systemOp :: Maybe SystemOp
  -- ^ What the instruction asks of the execution environment; 'Nothing' unless @SYSTEM@.
  }
  deriving (Generic, NFDataX)

isMemOp :: InstCtrl -> Bool
isMemOp InstCtrl {access} = isJust access

isLoad :: InstCtrl -> Bool
isLoad InstCtrl {access = Just (Load _ _)} = True
isLoad _ = False

isStore :: InstCtrl -> Bool
isStore InstCtrl {access = Just (Store _)} = True
isStore _ = False

isBranchOp :: InstCtrl -> Bool
isBranchOp InstCtrl {branch} = isJust branch

isCsrRead :: InstCtrl -> Bool
isCsrRead InstCtrl {systemOp = Just (SysCsr _)} = True
isCsrRead _ = False

-- | Whether the instruction form actually reads that source register.
usesRs1, usesRs2 :: InstCtrl -> Bool
usesRs1 InstCtrl {itype} = case itype of
  UType -> False
  JType -> False
  _ -> True
usesRs2 InstCtrl {itype} = case itype of
  RType -> True
  SType -> True
  BType -> True
  _ -> False
