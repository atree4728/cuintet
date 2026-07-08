module Tests.Cuintet.Core where

import Clash.Prelude

import Cuintet (system)
import Cuintet.Debug (showInstLogs)
import Cuintet.Eei
import Test.Tasty
import Test.Tasty.HUnit

-- | Contents of sample.hex.
sampleHex :: Vec 3 (BitVector ILen)
sampleHex =
  0x02000093 -- addi x1, x0, 32
    :> 0x00100117 -- auipc x2, 256
    :> 0x002081b3 -- add x3, x1, x2
    :> Nil

tests :: TestTree
tests =
  testGroup
    "Cuintet.Core"
    [ testCase
        "fetches sample.hex in order"
        $ do
          -- reset, mem delay, fifo write reg, fifo read delay, then 0, 4, 8
          showInstLogs (sampleN @System 7 $ system (blockRam sampleHex))
            @?= "(Nothing)\n\
                \(Nothing)\n\
                \(Nothing)\n\
                \(Nothing)\n\
                \00000000 : 02000093\n\
                \  itype   : 000010\n\
                \  imm     : 00000020\n\
                \  rs1[ 0] : 00000000\n\
                \  rs2[ 0] : 00000000\n\
                \  op1     : 00000000\n\
                \  op2     : 00000020\n\
                \  alu res : 00000020\n\
                \00000004 : 00100117\n\
                \  itype   : 010000\n\
                \  imm     : 00100000\n\
                \  rs1[ 0] : 00000000\n\
                \  rs2[ 1] : 00000001\n\
                \  op1     : 00000004\n\
                \  op2     : 00100000\n\
                \  alu res : 00100004\n\
                \00000008 : 002081b3\n\
                \  itype   : 000001\n\
                \  imm     : 00000000\n\
                \  rs1[ 1] : 00000001\n\
                \  rs2[ 2] : 00000002\n\
                \  op1     : 00000001\n\
                \  op2     : 00000002\n\
                \  alu res : 00000003"
    ]
