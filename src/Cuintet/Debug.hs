module Cuintet.Debug where

import Clash.Prelude
import Cuintet.Core (InstLog (..))
import Cuintet.Corectrl
import qualified Data.List as L
import Text.Printf (printf)

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
