module Tests.Cuintet.Core where

import Clash.Prelude
import qualified Prelude as P

import Cuintet.Core (InstLog (..))
import Cuintet.Eei (Inst)
import Test.Tasty
import Test.Tasty.HUnit
import Tests.Cuintet.Sim (finalRegs, runProgram)

-- | ALU 命令のみ。フェッチ順序の検査に使う。
aluProg :: [Inst]
aluProg =
  [ 0x02000093 -- addi x1, x0, 32
  , 0x00100117 -- auipc x2, 256
  , 0x002081b3 -- add  x3, x1, x2
  ]

{- | store した値を load で読み戻し、その次の命令で使う。
アドレス 0x100 はフェッチが到達しない位置にあるので、書き込んだデータを命令として
取り込むことはない。
-}
loadStoreProg :: [Inst]
loadStoreProg =
  [ 0x02a00093 -- addi x1, x0, 42
  , 0x10102023 -- sw   x1, 256(x0)
  , 0x10002103 -- lw   x2, 256(x0)
  , 0x00110193 -- addi x3, x2, 1
  ]

-- | x0 に書こうとする命令。x0 は常に 0 でなければならない。
x0Prog :: [Inst]
x0Prog =
  [ 0x00500013 -- addi x0, x0, 5
  , 0x000000b3 -- add  x1, x0, x0
  ]

tests :: TestTree
tests =
  testGroup
    "Cuintet.Core"
    [ testCase "命令を順に 1 回ずつ commit する" $ do
        -- プログラム末尾の 1 命令先（NOP パディング）まで見て、最後の命令が
        -- 二重 commit されていない（pc が 12 に進む）ことを検査する。
        let logs = runProgram 4 aluProg
        P.length logs @?= 4
        ((.pc) <$> logs) @?= [0, 4, 8, 12]
        (finalRegs 3 aluProg !! (3 :: Int)) @?= 0x00100024
    , testCase "store した値を load で読み戻し、次の命令で使える" $ do
        let logs = runProgram 5 loadStoreProg
            regs = finalRegs 4 loadStoreProg
        P.length logs @?= 5
        ((.pc) <$> logs) @?= [0, 4, 8, 12, 16]
        (regs !! (2 :: Int)) @?= 42
        (regs !! (3 :: Int)) @?= 43
    , testCase "x0 への書き戻しは無視される" $ do
        let regs = finalRegs 2 x0Prog
        (regs !! (0 :: Int)) @?= 0
        (regs !! (1 :: Int)) @?= 0
    ]
