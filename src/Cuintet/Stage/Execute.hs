module Cuintet.Stage.Execute (execute) where

import Clash.Prelude
import Cuintet.Alu (alu)
import Cuintet.BrUnit (brUnit)
import Cuintet.CoreCtrl (InstCtrl (..), InstType (..))
import Cuintet.Eei (Addr, XLen)
import Cuintet.Pipeline (ExMa (..), IdEx (..))

-- | Extract the two operands according to the instruction form.
operands ::
  InstCtrl ->
  BitVector XLen ->
  BitVector XLen ->
  BitVector XLen ->
  Addr ->
  (BitVector XLen, BitVector XLen)
operands InstCtrl {itype} imm rs1Data rs2Data pc = case itype of
  RType -> (rs1Data, rs2Data)
  BType -> (rs1Data, rs2Data)
  IType -> (rs1Data, imm)
  SType -> (rs1Data, imm)
  UType -> (bitCoerce pc, imm)
  JType -> (bitCoerce pc, imm)

execute :: IdEx -> ExMa
execute IdEx {..} = ExMa {op1, op2, aluResult, branchTaken, ..}
  where
    (op1, op2) = operands ctrl imm rs1Data rs2Data pc
    aluResult = alu ctrl op1 op2

    branchTaken = brUnit ctrl.funct3 op1 op2

    wbData
      | ctrl.isLui = imm
      | ctrl.isJump = bitCoerce (pc + 4)
      | otherwise = aluResult
