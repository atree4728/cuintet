-- | RISC-V execution environment interface.
module Cuintet.Eei where

import Clash.Annotations.BitRepresentation
import Clash.Annotations.BitRepresentation.Deriving
import Clash.Prelude
import Cuintet.Util (downto)

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

-- | Whether a narrower-than-word load fills the high bits with its sign or zero.
data Sign = Signed | Unsigned
  deriving (Generic, NFDataX, Show)
deriveDefaultAnnotation [t|Sign|]
deriveBitPack [t|Sign|]

{- | The width of a memory access, laid out so that it /is/ the @funct3@ field
of a load: @unpack funct3@ is pure wiring, and the sign comes along with it.

Stores use the same encoding but ignore the 'Sign'; @funct3@ bit 2 is reserved
in a store, so 'Unsigned' never reaches the bus.
-}
data AccessWidth
  = Byte Sign
  | Half Sign
  | Word
  | -- | @0b011@; @LD@ in RV64I.
    WidthIllegal1
  | -- | @0b110@; @LWU@ in RV64I.
    WidthIllegal2
  | -- | @0b111@.
    WidthIllegal3
  deriving (Generic, NFDataX, Show)
{-# ANN
  module
  ( DataReprAnn
      $(liftQ [t|AccessWidth|])
      3
      [ ConstrRepr 'Byte (1 `downto` 0) 0b00 [0b100]
      , ConstrRepr 'Half (1 `downto` 0) 0b01 [0b100]
      , ConstrRepr 'Word (2 `downto` 0) 0b010 []
      , ConstrRepr 'WidthIllegal1 (2 `downto` 0) 0b011 []
      , ConstrRepr 'WidthIllegal2 (2 `downto` 0) 0b110 []
      , ConstrRepr 'WidthIllegal3 (2 `downto` 0) 0b111 []
      ]
  )
  #-}
deriveBitPack [t|AccessWidth|]

-- | The byte offset of an access within its word, the lane 0 being the least significant.
type LaneOffset = Index XLenBytes

laneOffset :: Addr -> LaneOffset
laneOffset a = numConvert (truncateB (pack a) :: BitVector (CLog 2 XLenBytes))

-- | The offset in bits, to shift a word into or out of place.
bitOffset :: LaneOffset -> Int
bitOffset off = 8 * numConvert off

-- | The size of the access, in bytes.
sizeBytes :: AccessWidth -> Index (XLenBytes + 1)
sizeBytes = \case
  Byte _ -> 1
  Half _ -> 2
  Word -> natToNum @XLenBytes
  WidthIllegal1 -> illegal
  WidthIllegal2 -> illegal
  WidthIllegal3 -> illegal
 where
  illegal = deepErrorX "sizeBytes: illegal access width"

{- | Whether the access is naturally aligned, i.e. contained in a single word.
@sizeBytes - 1@ is exactly the mask of offset bits that must be zero.
-}
aligned :: AccessWidth -> LaneOffset -> Bool
aligned width off = pack off .&. mask == 0
 where
  mask :: BitVector (CLog 2 XLenBytes)
  mask = truncateB (pack (sizeBytes width - 1))

{- | The byte lanes the access covers, the lane 0 being the least significant:
@sizeBytes@ ones shifted up by the offset.
-}
laneMask :: forall nBytes. (KnownNat nBytes) => AccessWidth -> LaneOffset -> Vec nBytes Bool
laneMask width off = reverse $ bitCoerce mask
 where
  mask, ones :: BitVector nBytes
  mask = ones `shiftL` numConvert off
  ones = complement (complement 0 `shiftL` numConvert (sizeBytes width))

-- | Data to be written, which is masked and divided into bytes.
newtype StoreLanes nBytes = StoreLanes (Vec nBytes (Maybe (BitVector 8)))
  deriving (Generic, NFDataX)

-- | Load request, which is to be sliced and extended.
data LoadFmt = LoadFmt {width :: AccessWidth, offset :: LaneOffset}
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
