module Tests.Cuintet.Core where

import Clash.Prelude

import Cuintet.Core (InstLog (..))
import Cuintet.Eei (Inst)
import Test.Tasty
import Test.Tasty.HUnit
import Tests.Cuintet.Sim (finalRegs, runProgram)

aluProg :: [Inst]
aluProg =
  [ 0x02000093 -- addi x1, x0, 32
  , 0x00100117 -- auipc x2, 256
  , 0x002081b3 -- add  x3, x1, x2
  ]

loadStoreProg :: [Inst]
loadStoreProg =
  [ 0x02a00093 -- addi x1, x0, 42
  , 0x10102023 -- sw   x1, 256(x0)
  , 0x10002103 -- lw   x2, 256(x0)
  , 0x00110193 -- addi x3, x2, 1
  ]

x0Prog :: [Inst]
x0Prog =
  [ 0x00500013 -- addi x0, x0, 5
  , 0x000000b3 -- add  x1, x0, x0
  ]

tests :: TestTree
tests =
  testGroup
    "Cuintet.Core"
    [ testCase "Commit each instruction once, in order" $ do
        ((.pc) <$> runProgram 8 aluProg) @?= [0, 4, 8, 12, 16, 20, 24, 28]
        (finalRegs 3 aluProg !! (3 :: Int)) @?= 0x00100024
    , testCase "Load the value that was stored using store" $ do
        ((.pc) <$> runProgram 8 loadStoreProg) @?= [0, 4, 8, 12, 16, 20, 24, 28]
        let regs = finalRegs 4 loadStoreProg
        (regs !! (2 :: Int)) @?= 42
        (regs !! (3 :: Int)) @?= 43
    , testCase "Ignore write back to x0" $ do
        ((.pc) <$> runProgram 2 x0Prog) @?= [0, 4]
        let regs = finalRegs 2 x0Prog
        (regs !! (0 :: Int)) @?= 0
        (regs !! (1 :: Int)) @?= 0
    ]
