module Cuintet.Debug where

import Clash.Prelude
import Cuintet.Core (InstLog)
import Cuintet.Corectrl
import qualified Data.List as L
import Text.Printf (printf)

showInstLog :: InstLog -> String
showInstLog (pc, inst, (InstCtrl{itype}, imm), rs1Addr, rs2Addr, rs1Data, rs2Data, (op1, op2), aluResult, wbReq) =
  L.intercalate
    "\n"
    ( [ printf "%08x : %08x" (toInteger pc) (toInteger inst)
      , printf "  itype   : %06b" (toInteger $ instCode itype)
      , printf "  imm     : %08x" (toInteger (if hasUndefined imm then zeroBits else imm))
      , printf "  rs1[%2d] : %08x" (toInteger rs1Addr) (toInteger rs1Data)
      , printf "  rs2[%2d] : %08x" (toInteger rs2Addr) (toInteger rs2Data)
      , printf "  op1     : %08x" (toInteger op1)
      , printf "  op2     : %08x" (toInteger op2)
      , printf "  alu res : %08x" (toInteger aluResult)
      ]
        L.++ maybe [] (\(rdAddr, wbData) -> [printf "  reg[%2d] <= %08x" (toInteger rdAddr) (toInteger wbData)]) wbReq
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
