module Cuintet.Debug (showInstLog, showInstLogs) where

import Clash.Prelude
import Cuintet.CoreCtrl (InstCtrl (itype), instCode)
import Cuintet.Pipeline (InstLog (..))
import Data.List qualified as L
import Text.Printf (printf)

{- | Displays the log for a single command in a human-friendly format. Write back is optional.

>>> import Prelude
>>> import Cuintet.Pipeline (InstLog (..))
>>> import Cuintet.CoreCtrl (InstCtrl (..), InstType (..))
>>> ctrl = InstCtrl{itype = IType, rwbEn = True, isLui = False, isAluOp = True, isOp32 = False, isJump = False, isLoad = False, systemOp = Nothing, funct3 = 0, funct7 = 0}
>>> l = InstLog{pc = 12, inst = 0x00110193, ctrl, imm = 1, rs1Addr = 2, rs2Addr = 1, rs1Data = 42, rs2Data = 0, op1 = 42, op2 = 1, aluResult = 43, wbReq = Just (3, 43), branchTaken = Nothing, csrRdata = Nothing}
>>> putStrLn (showInstLog l)
0000000c : 00110193
  itype   : 000010
  imm     : 00000001
  rs1[ 2] : 0000002a
  rs2[ 1] : 00000000
  op1     : 0000002a
  op2     : 00000001
  alu res : 0000002b
  reg[ 3] <= 0000002b

@wbReq@ is optional:

>>> putStrLn (showInstLog l{wbReq = Nothing})
0000000c : 00110193
  itype   : 000010
  imm     : 00000001
  rs1[ 2] : 0000002a
  rs2[ 1] : 00000000
  op1     : 0000002a
  op2     : 00000001
  alu res : 0000002b
-}
showInstLog :: InstLog -> String
showInstLog l =
  L.intercalate
    "\n"
    ( [ printf "%08x : %08x" (toInteger l.pc) (toInteger l.inst)
      , printf "  itype   : %06b" (toInteger $ instCode l.ctrl.itype)
      , printf "  imm     : %08x" (toInteger (if hasUndefined l.imm then zeroBits else l.imm))
      , printf "  rs1[%2d] : %08x" (toInteger l.rs1Addr) (toInteger l.rs1Data)
      , printf "  rs2[%2d] : %08x" (toInteger l.rs2Addr) (toInteger l.rs2Data)
      , printf "  op1     : %08x" (toInteger l.op1)
      , printf "  op2     : %08x" (toInteger l.op2)
      , printf "  alu res : %08x" (toInteger l.aluResult)
      ]
        L.++ maybe [] (\branchTaken -> [printf "  br take : %s" (show branchTaken)]) l.branchTaken
        L.++ maybe [] (\(rdAddr, wbData) -> [printf "  reg[%2d] <= %08x" (toInteger rdAddr) (toInteger wbData)]) l.wbReq
        L.++ maybe [] (\csrRdata -> [printf "  csr rdata : %08x" (toInteger csrRdata)]) l.csrRdata
    )

showInstLogs :: [Maybe InstLog] -> String
showInstLogs xs = L.intercalate "\n" $ L.map (maybe "(Nothing)" showInstLog) xs
