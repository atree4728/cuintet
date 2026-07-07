module Tests.Cuintet.Core where

import Clash.Prelude

import Cuintet (system)
import Cuintet.Debug (showInstLogs)
import Cuintet.Eei
import Test.Tasty
import Test.Tasty.HUnit

-- | Contents of sample.hex.
sampleHex :: Vec 4 (BitVector ILen)
sampleHex = 0x01234567 :> 0x89abcdef :> 0xdeadbeef :> 0xcafebebe :> Nil

tests :: TestTree
tests =
  testGroup
    "Cuintet.Core"
    [ testCase
        "fetches sample.hex in order"
        $ do
          -- reset, mem delay, fifo write reg, fifo read delay, then 0, 4, 8, 12
          showInstLogs (sampleN @System 7 $ system (blockRam sampleHex))
            @?= "(Nothing)\n\
                \(Nothing)\n\
                \(Nothing)\n\
                \(Nothing)\n\
                \00000000 : 01234567\n\
                \  itype   : 000010\n\
                \  imm     : 00000012\n\
                \  rs1[ 6] : 00000006\n\
                \  rs2[18] : 00000012\n\
                \00000004 : 89abcdef\n\
                \  itype   : 100000\n\
                \  imm     : fffbc09a\n\
                \  rs1[23] : 00000017\n\
                \  rs2[26] : 0000001a\n\
                \00000008 : deadbeef\n\
                \  itype   : 100000\n\
                \  imm     : fffdb5ea\n\
                \  rs1[27] : 0000001b\n\
                \  rs2[10] : 0000000a"
    ]
