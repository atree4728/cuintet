-- | RISC-V execution environment interface.
module Cuintet.Eei where

import Clash.Prelude

-- | The length of integer registers.
type XLen = 32

-- | The maximum width of instructions which the implementation supports.
type ILen = 32

type Addr = Unsigned XLen

type Inst = BitVector ILen

data MemBusReq dataWidth addrWidth = MemBusReq
  { valid :: Bool
  -- ^ Whether to request memory access.
  , addr :: Unsigned addrWidth
  -- ^ The address to access.
  , wdata :: Maybe (BitVector dataWidth)
  -- ^ Data to write.
  }
  deriving (Generic, NFDataX)

data MemBusResp dataWidth = MemBusResp
  { ready :: Bool
  -- ^ Whether to accept a memory access request.
  , rdata :: Maybe (BitVector dataWidth)
  -- ^ Data read.
  }
  deriving (Generic, NFDataX)

type MemDataWidth = 32
type MemAddrWidth = 20
type MemAddr = Unsigned MemAddrWidth

addrToMemAddr :: Addr -> MemAddr
addrToMemAddr a = truncateB (a `shiftR` natToNum @(CLog 2 (MemDataWidth `Div` 8)))

data OpCode
  = Lui
  | AuiPc
  | Op
  | OpImm
  | Jal
  | Jalr
  | Branch
  | Load
  | Store
  deriving (Generic, NFDataX)

{- ORMOLU_DISABLE -}
opEncode :: OpCode -> BitVector 7
opEncode Lui    = 0b0110111
opEncode AuiPc  = 0b0010111
opEncode Op     = 0b0110011
opEncode OpImm  = 0b0010011
opEncode Jal    = 0b1101111
opEncode Jalr   = 0b1100111
opEncode Branch = 0b1100011
opEncode Load   = 0b0000011
opEncode Store  = 0b0100011
{- ORMOLU_ENABLE -}

opDecode :: BitVector 7 -> OpCode
opDecode 0b0110111 = Lui
opDecode 0b0010111 = AuiPc
opDecode 0b0110011 = Op
opDecode 0b0010011 = OpImm
opDecode 0b1101111 = Jal
opDecode 0b1100111 = Jalr
opDecode 0b1100011 = Branch
opDecode 0b0000011 = Load
opDecode 0b0100011 = Store
opDecode _ = deepErrorX "encode: unknown opcode"
