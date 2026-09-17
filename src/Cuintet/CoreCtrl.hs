module Cuintet.CoreCtrl (
  InstFormat (..),
  InstCtrl (..),
  isLoad,
  isStore,
  isCsrRead,
  usesRs1,
  usesRs2,
  isJalr,
  OpClass (..),
  opClassOf,
  NExecUnits,
  ExecUnit (..),
  execUnit,
  Wakeup (..),
  wakeup,
  fitsPort,
) where

import Clash.Prelude
import Cuintet.Eei (AluOp, BranchOp, IssueWidth, MemOp, MulDivOp, SystemOp (..))
import Cuintet.Eei qualified as Eei (MemOp (Load, Store))
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
isLoad InstCtrl {memOp = Just (Eei.Load _ _)} = True
isLoad _ = False

isStore :: InstCtrl -> Bool
isStore InstCtrl {memOp = Just (Eei.Store _)} = True
isStore _ = False

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

data OpClass = Alu | Branch | Jal | Jalr | Csr | MulDiv | Load | Store
  deriving (Generic, NFDataX, Eq)

opClassOf :: InstCtrl -> OpClass
opClassOf ctrl
  | isLoad ctrl = Load
  | isStore ctrl = Store
  | isJust ctrl.mulDivOp = MulDiv
  | isJust ctrl.systemOp = Csr
  | isJalr ctrl = Jalr
  | ctrl.isJump = Jal
  | isJust ctrl.branchOp = Branch
  | otherwise = Alu

data ExecUnit = MulDivUnit | MemUnit
  deriving (Generic, NFDataX, Eq, Enum)

type NExecUnits = 2

execUnit :: OpClass -> Maybe ExecUnit
execUnit MulDiv = Just MulDivUnit
execUnit Load = Just MemUnit
execUnit _ = Nothing

-- | When an 'OpClass' broadcasts its destination tag.
data Wakeup = AtIssue | AtComplete | AtCommit
  deriving (Generic, NFDataX, Eq)

wakeup :: OpClass -> Wakeup
wakeup Csr = AtCommit
wakeup MulDiv = AtComplete
wakeup Load = AtComplete
wakeup _ = AtIssue

fitsPort :: Index IssueWidth -> OpClass -> Bool
fitsPort port opClass
  | port == 0 = True
  | otherwise = case opClass of
      Alu -> True
      Branch -> True
      Jal -> True
      _ -> False
