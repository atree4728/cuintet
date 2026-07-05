-- | RISC-V execution environment interface.
module Cuintet.Eei where

import Clash.Prelude

-- | The length of integer registers.
type XLen = 32

-- | The maximum width of instructions which the implementation supports.
type ILen = 32

type Addr = Unsigned XLen

type Inst = Unsigned ILen

data MemBusReq dataWidth addrWidth = MemBusReq
  { valid :: Bool
  -- ^ Whether to request memory access.
  , addr :: Unsigned addrWidth
  -- ^ The address to access.
  , wen :: Bool
  -- ^ Write enable.
  , wdata :: BitVector dataWidth
  -- ^ Data to write.
  }
  deriving (Generic, NFDataX)

data MemBusResp dataWidth = MemBusResp
  { ready :: Bool
  -- ^ Whether to accept a memory access request.
  , rvalid :: Bool
  -- ^ Whether processing of an accepted request has been completed.
  , rdata :: BitVector dataWidth
  -- ^ Data read.
  }
  deriving (Generic, NFDataX)

type MemDataWidth = 32
type MemAddrWidth = 20

addrToMemAddr :: Addr -> Unsigned MemAddrWidth
addrToMemAddr a = truncateB (a `shiftR` natToNum @(CLog 2 (MemDataWidth `Div` 8)))
