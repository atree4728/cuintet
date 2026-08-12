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
  BusReq (..),
  BusResp (..),
  MemDataBytes,
  MemReq,
  MemResp,
  Opcode (LUI, AUIPC, JAL, JALR, BRANCH, LOAD, STORE, OP_IMM, OP_REG, OP_IMM_32, OP_REG_32, MISC_MEM, SYSTEM),
  IOp (..),
  ShiftRight (..),
  MulDivType (..),
  CsrType (..),
  CsrOp (..),
  System12 (System12, ECALL, MRET),
  SystemOp (..),
  instAt,
  RegFile,
  RegAddr,
) where

import Clash.Annotations.BitRepresentation
import Clash.Annotations.BitRepresentation.Deriving
import Clash.Prelude
import Cuintet.Util (downto)

-- | The length of integer registers.
type XLen = 64

-- | The maximum width of instructions which the implementation supports.
type ILen = 32

-- | Widths of the buses are counted in bytes; @* 8@ appears only where a byte lane vector is turned back into a word.
type XLenBytes = XLen `Div` 8

{- | A physical memory address, counted in bytes as the ISA has it, and as wide as a register.

It says nothing about how the memory behind it is built: the bus word width and
the byte lanes are the memory's business, and an address keeps naming the same
byte whatever they are.
-}
type Addr = Unsigned XLen

-- | An instruction word. RV64I has the 32-bit form only.
type Inst = BitVector ILen

-- | The integer registers. @x0@ is kept zero by never being written, so reading it needs no special case.
type RegFile = Vec 32 (BitVector XLen)

-- | A register index; the @rs1@, @rs2@ or @rd@ field verbatim.
type RegAddr = BitVector 5

-- | Whether a narrower-than-register load fills the high bits with its sign or zero.
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

{- | The offset of the address within its word, i.e. the low bits of it that the
memory itself ignores.
-}
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

-- | Whether the access is naturally aligned, i.e. contained in a single word. @sizeBytes - 1@ is exactly the mask of offset bits that must be zero.
aligned :: AccessWidth -> LaneOffset -> Bool
aligned width off = pack off .&. mask == 0
  where
    mask :: BitVector (CLog 2 XLenBytes)
    mask = truncateB (pack (sizeBytes width - 1))

-- | The byte lanes the access covers, the lane 0 being the least significant: @sizeBytes@ ones shifted up by the offset.
laneMask :: forall nBytes. (KnownNat nBytes) => AccessWidth -> LaneOffset -> Vec nBytes Bool
laneMask width off = reverse $ bitCoerce mask
  where
    mask, ones :: BitVector nBytes
    mask = ones `shiftL` numConvert off
    ones = complement (complement 0 `shiftL` numConvert (sizeBytes width))

-- | The instruction sitting at the address, picked out of the bus word that contains it.
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
data BusReq nBytes = BusReq
  { addr :: Addr
  -- ^ The address to access.
  , wdata :: Maybe (StoreLanes nBytes)
  -- ^ 'Just' the data to write for stores, 'Nothing' for loads.
  }
  deriving (Generic, NFDataX)

{- | The memory's half of the bus: whether it takes a request this cycle, and
the word read for one it took earlier. The two are independent, so a request may
go out while the answer to the previous one is still coming back.
-}
data BusResp nBytes = BusResp
  { ready :: Bool
  -- ^ Whether to accept a memory access request.
  , rdata :: Maybe (BitVector (nBytes * 8))
  -- ^ Data read.
  }
  deriving (Generic, NFDataX)

{- | The width of the memory bus, in bytes. One register wide, so a naturally
aligned access is always contained in a single bus word.
-}
type MemDataBytes = XLenBytes

-- | 'BusReq' at the width the memory bus is.
type MemReq = BusReq MemDataBytes

-- | 'BusResp' at the width the memory bus is.
type MemResp = BusResp MemDataBytes

{- | The @opcode@ field. RV64I names only a handful of the 128 patterns, so this
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

data MulOp = MulLow | MulHighHom Sign | MulHighHetero
  deriving (Generic, NFDataX)

data DivOp = Div Sign | Rem Sign
  deriving (Generic, NFDataX)

data MulDivType = Multiply MulOp | Division DivOp
  deriving (Generic, NFDataX)

{-# ANN
  module
  ( DataReprAnn
      $(liftQ [t|MulOp|])
      2
      [ ConstrRepr 'MulLow 0b11 0b00 []
      , ConstrRepr 'MulHighHom 0b01 0b01 [0b10]
      , ConstrRepr 'MulHighHetero 0b11 0b10 []
      ]
  )
  #-}

deriveBitPack [t|MulOp|]

{-# ANN
  module
  ( DataReprAnn
      $(liftQ [t|DivOp|])
      2
      [ ConstrRepr 'Div 0b10 0b00 [0b01]
      , ConstrRepr 'Rem 0b10 0b10 [0b01]
      ]
  )
  #-}

deriveBitPack [t|DivOp|]

{-# ANN
  module
  ( DataReprAnn
      $(liftQ [t|MulDivType|])
      3
      [ ConstrRepr 'Multiply 0b100 0b000 [0b011]
      , ConstrRepr 'Division 0b100 0b100 [0b011]
      ]
  )
  #-}

deriveBitPack [t|MulDivType|]

{- | What a CSR access does to the register, laid out as @funct3@ bits 1-0. The
fourth pattern is the @funct3@ that names no CSR instruction.
-}
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

{- | A CSR access: what it does, and where its operand comes from. Laid out so
that it /is/ the @funct3@ field, bit 2 choosing between @rs1@ and the 5-bit
immediate that takes its place.
-}
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
