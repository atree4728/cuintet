-- | 'Cuintet.Core.InstLog' の整形。将来の Konata ログ出力の土台にする。
module Cuintet.Debug where

import Clash.Prelude
import Cuintet.Core (InstLog (..))
import Cuintet.Corectrl
import qualified Data.List as L
import Text.Printf (printf)

{- | 1 命令分のログを人間可読な複数行テキストにする。書き戻しのある命令では
最後に @reg[rd] <= data@ の行が付く。

>>> import Prelude
>>> import Cuintet.Core (InstLog (..))
>>> import Cuintet.Corectrl (InstCtrl (..), InstType (..))
>>> ctrl = InstCtrl{itype = IType, rwbEn = True, isLui = False, isAluOp = True, isJump = False, isLoad = False, funct3 = 0, funct7 = 0}
>>> l = InstLog{pc = 12, inst = 0x00110193, ctrl, imm = 1, rs1Addr = 2, rs2Addr = 1, rs1Data = 42, rs2Data = 0, op1 = 42, op2 = 1, aluResult = 43, wbReq = Just (3, 43)}
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

@wbReq@ が 'Nothing' なら、その行は出ない:

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
        L.++ maybe [] (\(rdAddr, wbData) -> [printf "  reg[%2d] <= %08x" (toInteger rdAddr) (toInteger wbData)]) l.wbReq
    )

showInstLogs :: [Maybe InstLog] -> String
showInstLogs xs =
  L.intercalate "\n" $
    L.map
      ( \case
          Just instLog -> showInstLog instLog
          Nothing -> "(Nothing)"
      )
      xs
