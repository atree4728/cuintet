module Cuintet.CoreCtrl (
  InstFormat (..),
  InstCtrl (..),
  isLoad,
  isStore,
  isCsr,
  usesRs1,
  usesRs2,
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
import Cuintet.Eei (AluOp, BranchOp, IssueWidth, MemOp, MulDivOp, NAluPorts, SystemOp (..))
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
  , writesRd :: Bool
  -- ^ Whether the instruction writes @rd@.
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

isCsr :: InstCtrl -> Bool
isCsr InstCtrl {systemOp = Just (SysCsr _)} = True
isCsr _ = False

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

data ExecUnit = MulDivUnit | LoadUnit
  deriving (Generic, NFDataX, Eq, Enum)

type NExecUnits = 2

execUnit :: OpClass -> Maybe ExecUnit
execUnit MulDiv = Just MulDivUnit
execUnit Load = Just LoadUnit
execUnit _ = Nothing

data Wakeup
  = -- | RR wakes, then EX and WB bypass until the register file has it.
    AtIssue
  | -- | a unit wakes and bypasses while it holds the completion.
    AtComplete
  deriving (Generic, NFDataX, Eq)

wakeup :: OpClass -> Wakeup
wakeup MulDiv = AtComplete
wakeup Load = AtComplete
wakeup _ = AtIssue

fitsPort :: Index IssueWidth -> OpClass -> Bool
fitsPort port = \case
  MulDiv -> port == aluPorts
  Load -> port == aluPorts + 1
  Store -> port == aluPorts + 2
  Csr -> port == 0
  _ -> port < aluPorts
  where
    aluPorts = natToNum @NAluPorts
