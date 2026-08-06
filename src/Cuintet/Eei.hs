-- | RISC-V execution environment interface.
module Cuintet.Eei (
  XLen,
  XLenBytes,
  ILen,
  Addr,
  Inst,
  Sign (..),
  AccessWidth (..),
  LaneOffset,
  laneOffset,
  bitOffset,
  sizeBytes,
  aligned,
  laneMask,
  StoreLanes (..),
  LoadFmt (..),
  MemBusReq (..),
  MemBusResp (..),
  MemDataBytes,
  MemReq,
  MemResp,
  Opcode (LUI, AUIPC, JAL, JALR, BRANCH, LOAD, STORE, OP_IMM, OP_REG, OP_IMM_32, OP_REG_32, MISC_MEM, SYSTEM),
  IOp (..),
  ShiftRight (..),
  CsrType (..),
  CsrOp (..),
  System12 (System12, ECALL, MRET),
  SystemOp (..),
  instAt,
) where

import Clash.Annotations.BitRepresentation
import Clash.Annotations.BitRepresentation.Deriving
import Clash.Prelude
import Cuintet.Util (downto)

-- | The length of integer registers.
type XLen = 64

-- | The maximum width of instructions which the implementation supports.
type ILen = 32

{- | Widths of the buses are counted in bytes; @* 8@ appears only where a byte
lane vector is turned back into a word.
-}
type XLenBytes = XLen `Div` 8

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
  | Word Sign
  | DoubleWord
  | WidthIllegal
  deriving (Generic, NFDataX, Show)

{-# ANN
  module
  ( DataReprAnn
      $(liftQ [t|AccessWidth|])
      3
      [ ConstrRepr 'Byte (1 `downto` 0) 0b00 [0b100]
      , ConstrRepr 'Half (1 `downto` 0) 0b01 [0b100]
      , ConstrRepr 'Word (1 `downto` 0) 0b10 [0b100]
      , ConstrRepr 'DoubleWord (2 `downto` 0) 0b011 []
      , ConstrRepr 'WidthIllegal (2 `downto` 0) 0b111 []
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
  Word _ -> 4
  DoubleWord -> 8
  WidthIllegal -> deepErrorX "sizeBytes: illegal access width"

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

instAt :: Addr -> BitVector (MemDataBytes * 8) -> Inst
instAt addr busWord = truncateB (busWord `shiftR` bitOffset (laneOffset addr))

-- | Data to be written, which is masked and divided into bytes.
newtype StoreLanes nBytes = StoreLanes (Vec nBytes (Maybe (BitVector 8)))
  deriving stock (Generic)
  deriving anyclass (NFDataX)

-- | Load request, which is to be sliced and extended.
data LoadFmt = LoadFmt {width :: AccessWidth, offset :: LaneOffset}
  deriving (Generic, NFDataX)

{- | Memory access request, carried on the bus as @Maybe (MemBusReq ...)@;
@Nothing@ means no access.
-}
data MemBusReq nBytes = MemBusReq
  { addr :: Addr
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

type MemDataBytes = XLenBytes

type MemReq = MemBusReq MemDataBytes

type MemResp = MemBusResp MemDataBytes

{- | The @opcode@ field. RV32I names only a handful of the 128 patterns, so this
is the field itself with names attached rather than a sum type: @unpack@ is pure
wiring, and one declaration serves as both the encoder and the decoder. Matching
on it needs a catch-all for the patterns left unnamed.
-}
newtype Opcode = Opcode (BitVector 7)
  deriving newtype (BitPack)

pattern LUI, AUIPC, JAL, JALR, BRANCH, LOAD, STORE, OP_IMM, OP_REG, MISC_MEM, SYSTEM, OP_REG_32, OP_IMM_32 :: Opcode
{- FOURMOLU_DISABLE -}
pattern LUI       = Opcode 0b0110111
pattern AUIPC     = Opcode 0b0010111
pattern JAL       = Opcode 0b1101111
pattern JALR      = Opcode 0b1100111
pattern BRANCH    = Opcode 0b1100011
pattern LOAD      = Opcode 0b0000011
pattern STORE     = Opcode 0b0100011
pattern OP_IMM    = Opcode 0b0010011
pattern OP_REG    = Opcode 0b0110011
pattern OP_IMM_32 = Opcode 0b0011011 
pattern OP_REG_32 = Opcode 0b0111011
pattern MISC_MEM  = Opcode 0b0001111
pattern SYSTEM    = Opcode 0b1110011
{- FOURMOLU_ENABLE -}

{- | The ALU operation of an @OP@ or @OP-IMM@ instruction, laid out so that it
/is/ the @funct3@ field: @unpack funct3@ is pure wiring. All 8 patterns are
named, so a match on it is total.

@SUB@ and @SRA@ are not here; they share their @funct3@ with 'ADD' and 'SR',
and are told apart by @funct7@ bit 5.
-}
data IOp
  = ADD
  | SLL
  | SLT
  | SLTU
  | XOR
  | SR
  | OR
  | AND
  deriving (Generic, NFDataX, Show)

deriveDefaultAnnotation [t|IOp|]
deriveBitPack [t|IOp|]

-- | Which right shift 'SR' means; @funct7@ bit 5.
data ShiftRight = Logical | Arithmetic
  deriving (Generic, NFDataX, Show)

deriveDefaultAnnotation [t|ShiftRight|]
deriveBitPack [t|ShiftRight|]

data CsrType
  = ReadWrite
  | ReadSet
  | ReadClear
  | CSRIllegal
  deriving (Generic, NFDataX)

{-# ANN
  module
  ( DataReprAnn
      $(liftQ [t|CsrType|])
      2
      [ ConstrRepr 'ReadWrite (1 `downto` 0) 0b01 []
      , ConstrRepr 'ReadSet (1 `downto` 0) 0b10 []
      , ConstrRepr 'ReadClear (1 `downto` 0) 0b11 []
      , ConstrRepr 'CSRIllegal (1 `downto` 0) 0b00 []
      ]
  )
  #-}

deriveBitPack [t|CsrType|]

data CsrOp
  = CsrReg CsrType
  | CsrImm CsrType
  deriving (Generic, NFDataX)

{-# ANN
  module
  ( DataReprAnn
      $(liftQ [t|CsrOp|])
      3
      [ ConstrRepr 'CsrReg (2 `downto` 2) 0b0 [0b011]
      , ConstrRepr 'CsrImm (2 `downto` 2) 0b1 [0b011]
      ]
  )
  #-}

deriveBitPack [t|CsrOp|]

{- | The @funct12@ field of a @SYSTEM@ instruction whose @funct3@ is zero, where
it names the operation rather than a CSR.
-}
newtype System12 = System12 (BitVector 12)
  deriving newtype (Eq)

pattern ECALL, MRET :: System12
pattern ECALL = System12 0
pattern MRET = System12 0b001100000010

{- | What a @SYSTEM@ instruction asks for. @funct3@ tells a CSR access from the
rest; among the rest, only @ECALL@ is implemented.
-}
data SystemOp
  = SysCsr CsrOp
  | SysEcall
  | SysMret
  | SysIllegal
  deriving (Generic, NFDataX)
