module Tests.Cuintet.Core (tests) where

import Clash.Prelude
import Cuintet.Eei (Inst)
import Cuintet.Pipeline (MaWb (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
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

loadProg :: [Inst]
loadProg =
  [ 0x02000083 -- lb   x1, 0x20(x0) : x1 = ffffffffffffffef
  , 0x02104103 -- lbu  x2, 0x21(x0) : x2 = 00000000000000be
  , 0x02201183 -- lh   x3, 0x22(x0) : x3 = ffffffffffffdead
  , 0x02205203 -- lhu  x4, 0x22(x0) : x4 = 000000000000dead
  , 0x00000013 -- nop
  , 0x00000013
  , 0x00000013
  , 0x00000013
  , 0xdeadbeef -- 0x20: the word the loads read
  ]

storeProg :: [Inst]
storeProg =
  [ 0x12300093 -- addi x1, x0, 0x123
  , 0x02101023 -- sh x1, 0x20(x0)
  , 0x02100123 -- sb x1, 0x22(x0)
  , 0x02200103 -- lb x2, 0x22(x0) : x2 = 00000023
  , 0x02001183 -- lh x3, 0x20(x0) : x3 = 00000123
  ]

jumpProg :: [Inst]
jumpProg =
  [ 0x0100006f --  0: jal x0, 0x10 : jump to 0x10
  , 0xdeadbeef --  4:
  , 0xdeadbeef --  8:
  , 0xdeadbeef --  c:
  , 0x01800093 -- 10: addi x1, x0, 0x18
  , 0x00808067 -- 14: jalr x0, 8(x1) : jump to x1+8=0x20
  , 0xdeadbeef -- 18:
  , 0xdeadbeef -- 1c:
  , 0xfe1ff06f -- 20: jal x0, -0x20 : jump to 0
  ]

branchProg :: [Inst]
branchProg =
  [ 0x00100093 --  0: addi x1, x0, 1
  , 0x10100063 --  4: beq x0, x1, 0x100 -- untaken
  , 0x00101863 --  8: bne x0, x1, 0x10  -- taken, jump to pc+0x10=0x18
  , 0xdeadbeef --  c:
  , 0xdeadbeef -- 10:
  , 0xdeadbeef -- 14:
  , 0x0000d063 -- 18: bge x1, x0, 0 -- taken, jump to itself
  ]

sltProg :: [Inst]
sltProg =
  [ 0xfff00093 -- addi  x1, x0, -1
  , 0x00100113 -- addi  x2, x0, 1
  , 0x0020a1b3 -- slt   x3, x1, x2 : -1 < 1          -> 1
  , 0x0020b233 -- sltu  x4, x1, x2 : ffffffffffffffff < 1 -> 0
  , 0x0000a293 -- slti  x5, x1, 0  : -1 < 0               -> 1
  , 0x0000b313 -- sltiu x6, x1, 0  : ffffffffffffffff < 0 -> 0
  ]

x0Prog :: [Inst]
x0Prog =
  [ 0x00500013 -- addi x0, x0, 5
  , 0x000000b3 -- add  x1, x0, x0
  ]

csrProg :: [Inst]
csrProg =
  [ 0x305bd0f3 -- 0: csrrwi x1, mtvec, 0b10111
  , 0x30502173 -- 4: csrrs  x2, mtvec, x0 -- 0b10100 (ignore mode bits)
  ]

ecallProg :: [Inst]
ecallProg =
  [ 0x30585073 --  0: csrrwi x0, mtvec, 0x10
  , 0x00000073 --  4: ecall
  , 0x00000000 --  8:
  , 0x00000000 --  c:
  , 0x342020f3 -- 10: csrrs x1, mcause, x0 -- 0xb (Environment call from M-mode])
  , 0x34102173 -- 14: csrrs x2, mepc, x0   -- trapped at 0x4
  ]

mretProg :: [Inst]
mretProg =
  [ 0x34185073 --  0: csrrwi x0, mepc, 0x10
  , 0x30200073 --  4: mret
  , 0x00000000 --  8:
  , 0x00000000 --  c:
  , 0x00000013 -- 10: addi x0, x0, 0
  ]

dataHazardProg :: [Inst]
dataHazardProg =
  [ 0x00100093 -- 0: addi x1, x0, 1
  , 0x00108113 -- 4: addi x2, x1, 1
  ]

loadUseProg :: [Inst]
loadUseProg =
  [ 0x02a00093 --  0: addi x1, x0, 42
  , 0x10102023 --  4: sw   x1, 256(x0)
  , 0x10002103 --  8: lw   x2, 256(x0)
  , 0x00110193 --  c: addi x3, x2, 1
  , 0x00118213 -- 10: addi x4, x3, 1
  ]

tests :: TestTree
tests =
  testGroup
    "Cuintet.Core"
    [ testCase "Commit each instruction once, in order" $ do
        -- ((.pc) <$> runProgram 8 aluProg) @?= [0, 4, 8, 12, 16, 20, 24, 28]
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
    , testCase "Properly handle variants of load instruction" $ do
        let regs = finalRegs 4 loadProg
        (regs !! (1 :: Int)) @?= 0xffffffffffffffef
        (regs !! (2 :: Int)) @?= 0x00000000000000be
        (regs !! (3 :: Int)) @?= 0xffffffffffffdead
        (regs !! (4 :: Int)) @?= 0x000000000000dead
    , testCase "Properly handle variants of store instruction" $ do
        let regs = finalRegs 5 storeProg
        (regs !! (2 :: Int)) @?= 0x00000023
        (regs !! (3 :: Int)) @?= 0x00000123
    , testCase "Set less than, signed and unsigned" $ do
        let regs = finalRegs 6 sltProg
        (regs !! (3 :: Int)) @?= 1
        (regs !! (4 :: Int)) @?= 0
        (regs !! (5 :: Int)) @?= 1
        (regs !! (6 :: Int)) @?= 0
    , testCase "Unconditional jump" $ do
        ((.pc) <$> runProgram 5 jumpProg) @?= [0x0, 0x10, 0x14, 0x20, 0x0]
    , testCase "Conditional jump" $ do
        ((.pc) <$> runProgram 5 branchProg) @?= [0x0, 0x04, 0x08, 0x18, 0x18]
    , testCase "Zicsr" $ do
        let regs = finalRegs 2 csrProg
        (regs !! (2 :: Int)) @?= 0b10100
    , testCase "ecall" $ do
        ((.pc) <$> runProgram 4 ecallProg) @?= [0x0, 0x04, 0x10, 0x14]
        let regs = finalRegs 4 ecallProg
        (regs !! (1 :: Int)) @?= 0xb
        (regs !! (2 :: Int)) @?= 0x4
    , testCase "mret" $
        ((.pc) <$> runProgram 3 mretProg) @?= [0x0, 0x04, 0x10]
    , testCase "Interlock a data dependency" $ do
        let regs = finalRegs 2 dataHazardProg
        (regs !! (2 :: Int)) @?= 2
    , testCase "Interlock a load-use dependency" $ do
        let regs = finalRegs 5 loadUseProg
        (regs !! (2 :: Int)) @?= 42
        (regs !! (3 :: Int)) @?= 43
        (regs !! (4 :: Int)) @?= 44
    ]
