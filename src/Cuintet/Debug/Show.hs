module Cuintet.Debug.Show (showInstLog, instLogLines, showInstLogs, hex) where

import Clash.Prelude
import Cuintet.CoreCtrl (InstCtrl (itype), instCode)
import Cuintet.Pipeline (MaWb (..), destReg)
import Data.List (intercalate)
import Text.Printf (printf)

{- | Displays the log for a single command in a human-friendly format. Write back is optional.

>>> import Prelude
>>> import Cuintet.Pipeline (MaWb (..))
>>> import Cuintet.CoreCtrl (InstCtrl (..), InstType (..))
>>> ctrl = InstCtrl{itype = IType, rwbEn = True, isLui = False, isAluOp = True, isOp32 = False, isJump = False, isLoad = False, mulDiv = Nothing, systemOp = Nothing, funct3 = 0, funct7 = 0}
>>> l = MaWb{pc = 12, instBits = 0x00110193, ctrl, imm = 1, rs1Addr = 2, rs2Addr = 1, rdAddr = 3, rs1Data = 42, rs2Data = 0, op1 = 42, op2 = 1, aluResult = 43, wbData = 43, branchTaken = Nothing, csrRdata = Nothing}
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

A field the instruction does not have is undefined, not zero:

>>> import Clash.Prelude (deepErrorX)
>>> lines (showInstLog l{rs2Data = deepErrorX "never written"}) !! 4
"  rs2[ 1] : xxxxxxxx"
-}
showInstLog :: MaWb -> String
showInstLog = intercalate "\n" . instLogLines

-- | 'showInstLog' one line at a time, for callers that put each on a line of their own.
instLogLines :: MaWb -> [String]
instLogLines l =
  [ printf "%s : %s" (hex l.pc) (hex l.instBits)
  , printf "  itype   : %06b" (toInteger $ instCode l.ctrl.itype)
  , printf "  imm     : %s" (hex l.imm)
  , printf "  rs1[%2d] : %s" (toInteger l.rs1Addr) (hex l.rs1Data)
  , printf "  rs2[%2d] : %s" (toInteger l.rs2Addr) (hex l.rs2Data)
  , printf "  op1     : %s" (hex l.op1)
  , printf "  op2     : %s" (hex l.op2)
  , printf "  alu res : %s" (hex l.aluResult)
  ]
    <> maybe [] (\branchTaken -> [printf "  br take : %s" (show branchTaken)]) l.branchTaken
    <> maybe [] (\rdAddr -> [printf "  reg[%2d] <= %s" (toInteger rdAddr) (hex l.wbData)]) (destReg l)
    <> maybe [] (\csrRdata -> [printf "  csr rdata : %s" (hex csrRdata)]) l.csrRdata

hex :: (BitPack a) => a -> String
hex a
  | hasUndefined bv = "xxxxxxxx"
  | otherwise = printf "%08x" (toInteger bv)
  where
    bv = pack a

showInstLogs :: [Maybe MaWb] -> String
showInstLogs xs = intercalate "\n" $ maybe "(Nothing)" showInstLog <$> xs
