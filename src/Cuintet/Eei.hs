-- | RISC-V execution environment interface.
module Cuintet.Eei where

import Clash.Prelude

-- | The length of integer registers.
type XLen = 32

-- | The maximum width of instructions which the implementation supports.
type ILen = 32

-- | Widths of the buses are counted in bytes; @* 8@ appears only where a byte
-- lane vector is turned back into a word.
type XLenBytes = XLen `Div` 8

type ILenBytes = ILen `Div` 8

type Addr = Unsigned XLen

type Inst = BitVector ILen

-- | The byte range an access covers, always aligned to its own size.
data ByteRange
  = -- | Byte i -> [(i+1)*8-1:i*8]
    Byte (Index XLenBytes)
  | -- | Half i -> [(i+1)*16-1:i*16]
    Half (Index (XLenBytes `Div` 2))
  | -- | Word   -> [31:0]
    Word
  deriving (Generic, NFDataX)

-- | The offset of the range, in bytes. The only place where a half word index
-- is scaled to bytes.
byteOffset :: ByteRange -> Index XLenBytes
byteOffset (Byte i) = i
byteOffset (Half i) = 2 * numConvert i
byteOffset Word = 0

-- | The offset of the range in bits, to shift a word into or out of place.
bitOffset :: ByteRange -> Int
bitOffset range = 8 * numConvert (byteOffset range)

-- | The size of the range, in bytes.
sizeBytes :: ByteRange -> Index (XLenBytes + 1)
sizeBytes (Byte _) = 1
sizeBytes (Half _) = 2
sizeBytes Word = natToNum @XLenBytes

{- | The byte lanes the range covers, the lane 0 being the least significant:
@sizeBytes@ ones shifted up by @byteOffset@.
-}
laneMask :: forall nBytes. (KnownNat nBytes) => ByteRange -> Vec nBytes Bool
laneMask range = reverse $ bitCoerce mask
 where
  mask, ones :: BitVector nBytes
  mask = ones `shiftL` numConvert (byteOffset range)
  ones = complement (complement 0 `shiftL` numConvert (sizeBytes range))

-- | Data to be written, which is masked and divided into bytes.
newtype StoreLanes nBytes = StoreLanes (Vec nBytes (Maybe (BitVector 8)))
  deriving (Generic, NFDataX)

-- | Load request, which is to be sliced and extended.
data LoadFmt = LoadFmt {range :: ByteRange, signed :: Bool}
  deriving (Generic, NFDataX)

{- | Memory access request, carried on the bus as @Maybe (MemBusReq ...)@;
@Nothing@ means no access.
-}
data MemBusReq nBytes addrWidth = MemBusReq
  { addr :: Unsigned addrWidth
  -- ^ The address to access.
  , wdata :: Maybe (StoreLanes nBytes)
  -- ^ 'Just' the data to write for stores, 'Nothing' for loads.
  }
  deriving (Generic, NFDataX)

data MemBusResp nBytes = MemBusResp
  { ready :: Bool
  -- ^ Whether to accept a memory access request.
  , rdata :: Maybe (BitVector (nBytes * 8))
  -- ^ Data read.
  }
  deriving (Generic, NFDataX)

type MemDataBytes = 4
type WordAddrWidth = 20

-- | The address the memory takes, indexing words rather than bytes.
type WordAddr = Unsigned WordAddrWidth

type InstResp = MemBusResp ILenBytes
type DataResp = MemBusResp MemDataBytes
type InstReq = MemBusReq ILenBytes XLen
type DataReq = MemBusReq MemDataBytes XLen

-- | Physical request form accepted by the memory.
type MemReq = MemBusReq MemDataBytes WordAddrWidth

toWordAddr :: Addr -> WordAddr
toWordAddr a = truncateB (a `shiftR` natToNum @(CLog 2 MemDataBytes))

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
