module Cuintet.Debug where

import Clash.Prelude
import Cuintet.Corectrl
import Cuintet.Eei
import qualified Data.List as L
import Text.Printf (printf)

type InstLog =
  ( Addr
  , Inst
  , ( InstCtrl
    , BitVector XLen
    )
  , BitVector 5
  , BitVector 5
  , BitVector XLen
  , BitVector XLen
  )

showInstLog :: InstLog -> String
showInstLog (pc, inst, (InstCtrl{itype}, imm), rs1Addr, rs2Addr, rs1Data, rs2Data) =
  L.intercalate
    "\n"
    [ printf "%08x : %08x" (toInteger pc) (toInteger inst)
    , printf "  itype   : %06b" (toInteger $ instCode itype)
    , printf "  imm     : %08x" (toInteger imm)
    , printf "  rs1[%2d] : %08x" (toInteger rs1Addr) (toInteger rs1Data)
    , printf "  rs2[%2d] : %08x" (toInteger rs2Addr) (toInteger rs2Data)
    ]

showInstLogs :: [Maybe InstLog] -> String
showInstLogs xs =
  L.intercalate "\n" $
    L.map
      ( \case
          Just instLog -> showInstLog instLog
          Nothing -> "(Nothing)"
      )
      xs
