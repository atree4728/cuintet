-- | RISC-V execution environment interface.
module Cuintet.Eei (
  XLen,
  XLenBytes,
  ILen,
  Addr,
  Inst,
  Sign (..),
  Width (..),
  Access (..),
  parseLoad,
  parseStore,
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
  AluOp (..),
  BranchCond (..),
  parseBranch,
  MulOp (..),
  DivOp (..),
  MulDivType (..),
  CsrOp (..),
  CsrSrc (..),
  parseCsr,
  System12 (System12, ECALL, EBREAK, MRET),
  SystemOp (..),
  instAt,
  RegFile,
  RegAddr,
  TrapCause (..),
  pattern ILLEGAL_INSTRUCTION,
  pattern BREAKPOINT,
  pattern ENVIRONMENT_CALL_FROM_M_MODE,
) where

import Clash.Annotations.BitRepresentation
import Clash.Annotations.BitRepresentation.Deriving
import Clash.Prelude
import Cuintet.Util (downto, orNothing)

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
  deriving (Generic, NFDataX, Eq, Show)

deriveDefaultAnnotation [t|Sign|]
deriveBitPack [t|Sign|]

{- | The width of a memory access, laid out so that it /is/ @funct3@ bits 1-0:
@unpack@ of them is pure wiring. All four patterns name a width, so a match on
this type is total.

The sign of a load is @funct3@ bit 2, orthogonal to the width, and is kept apart
as a 'Sign'.
-}
data Width = B | H | W | D
  deriving (Generic, NFDataX, Show)

deriveDefaultAnnotation [t|Width|]
deriveBitPack [t|Width|]

-- | What a memory instruction asks of memory. A store carries no 'Sign': @funct3@ bit 2 is reserved in one.
data Access = Load Width Sign | Store Width
  deriving (Generic, NFDataX)

{- | The width and sign a load's @funct3@ names, or 'Nothing' for @0b111@, the
one pattern that names no load.

>>> (parseLoad 0b001, parseLoad 0b101)  -- lh, lhu
(Just (H,Signed),Just (H,Unsigned))
>>> parseLoad 0b111
Nothing
-}
parseLoad :: BitVector 3 -> Maybe (Width, Sign)
parseLoad f3 = orNothing (f3 /= 0b111) (unpack (slice d1 d0 f3), unpack (slice d2 d2 f3))

{- | The width a store's @funct3@ names, or 'Nothing' when bit 2 is set: it is
reserved in a store, so none of @0b1xx@ names one.

>>> (parseStore 0b010, parseStore 0b110)  -- sw, and the reserved pattern beside it
(Just W,Nothing)
-}
parseStore :: BitVector 3 -> Maybe Width
parseStore f3 = orNothing (slice d2 d2 f3 == 0) (unpack (slice d1 d0 f3))

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
sizeBytes :: Width -> Index (XLenBytes + 1)
sizeBytes = \case
  B -> 1
  H -> 2
  W -> 4
  D -> 8

-- | Whether the access is naturally aligned, i.e. contained in a single word. @sizeBytes - 1@ is exactly the mask of offset bits that must be zero.
aligned :: Width -> LaneOffset -> Bool
aligned width off = pack off .&. mask == 0
  where
    mask :: BitVector (CLog 2 XLenBytes)
    mask = truncateB (pack (sizeBytes width - 1))

-- | The byte lanes the access covers, the lane 0 being the least significant: @sizeBytes@ ones shifted up by the offset.
laneMask :: forall nBytes. (KnownNat nBytes) => Width -> LaneOffset -> Vec nBytes Bool
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
data LoadFmt = LoadFmt {width :: Width, sign :: Sign, offset :: LaneOffset}
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
/is/ @funct3@ with @inst[30]@ under it: @unpack (funct3 ++# inst[30])@ is pure
wiring. That one bit is what tells 'SUB' from 'ADD' and 'SRA' from 'SRL', in
@OP@ and @OP-IMM@ alike, and in their 32-bit forms too.

The six four-bit patterns that name no operation have no constructor here, so a
match on this type is total. ID is what rejects them, and it is also what forces
the bit low in the forms where it belongs to the immediate.
-}
data AluOp
  = ADD
  | SUB
  | SLL
  | SLT
  | SLTU
  | XOR
  | SRL
  | SRA
  | OR
  | AND
  deriving (Generic, NFDataX, Show)

{-# ANN
  module
  ( DataReprAnn
      $(liftQ [t|AluOp|])
      4
      [ ConstrRepr 'ADD (3 `downto` 0) 0b0000 []
      , ConstrRepr 'SUB (3 `downto` 0) 0b0001 []
      , ConstrRepr 'SLL (3 `downto` 0) 0b0010 []
      , ConstrRepr 'SLT (3 `downto` 0) 0b0100 []
      , ConstrRepr 'SLTU (3 `downto` 0) 0b0110 []
      , ConstrRepr 'XOR (3 `downto` 0) 0b1000 []
      , ConstrRepr 'SRL (3 `downto` 0) 0b1010 []
      , ConstrRepr 'SRA (3 `downto` 0) 0b1011 []
      , ConstrRepr 'OR (3 `downto` 0) 0b1100 []
      , ConstrRepr 'AND (3 `downto` 0) 0b1110 []
      ]
  )
  #-}

deriveBitPack [t|AluOp|]

{- | The branch condition, laid out so that it /is/ the @funct3@ field of a
branch: @unpack funct3@ is pure wiring.

The two @funct3@ patterns that name no branch have no constructor here, so a
match on this type is total. 'parseBranch' is the only way in, and it is what
rejects them.
-}
data BranchCond
  = BEQ
  | BNE
  | BLT
  | BGE
  | BLTU
  | BGEU
  deriving (Generic, NFDataX, Show)

{-# ANN
  module
  ( DataReprAnn
      $(liftQ [t|BranchCond|])
      3
      [ ConstrRepr 'BEQ (2 `downto` 0) 0b000 []
      , ConstrRepr 'BNE (2 `downto` 0) 0b001 []
      , ConstrRepr 'BLT (2 `downto` 0) 0b100 []
      , ConstrRepr 'BGE (2 `downto` 0) 0b101 []
      , ConstrRepr 'BLTU (2 `downto` 0) 0b110 []
      , ConstrRepr 'BGEU (2 `downto` 0) 0b111 []
      ]
  )
  #-}

deriveBitPack [t|BranchCond|]

-- | The branch a @funct3@ names, or 'Nothing' for the two patterns that name none: @0b01x@.
parseBranch :: BitVector 3 -> Maybe BranchCond
parseBranch f3 = orNothing (slice d2 d1 f3 /= 0b01) (unpack f3)

data MulOp = MulLow | MulHighHom Sign | MulHighHetero
  deriving (Eq, Generic, NFDataX)

data DivOp = Div Sign | Rem Sign
  deriving (Eq, Generic, NFDataX)

data MulDivType = Multiply MulOp | Division DivOp
  deriving (Eq, Generic, NFDataX)

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

{- | What a CSR access does to the register, laid out as @funct3@ bits 1-0.

@0b00@ names no CSR instruction and has no constructor here, so a match on this
type is total. 'parseCsr' is the only way in, and it is what rejects it.
-}
data CsrOp
  = ReadWrite
  | ReadSet
  | ReadClear
  deriving (Generic, NFDataX, Show)

{-# ANN
  module
  ( DataReprAnn
      $(liftQ [t|CsrOp|])
      2
      [ ConstrRepr 'ReadWrite (1 `downto` 0) 0b01 []
      , ConstrRepr 'ReadSet (1 `downto` 0) 0b10 []
      , ConstrRepr 'ReadClear (1 `downto` 0) 0b11 []
      ]
  )
  #-}

deriveBitPack [t|CsrOp|]

{- | Where the operand of a CSR access comes from, laid out as @funct3@ bit 2:
either @rs1@ or the 5-bit immediate that takes its place.
-}
data CsrSrc = FromRs1 | FromUimm
  deriving (Generic, NFDataX, Show)

{-# ANN
  module
  ( DataReprAnn
      $(liftQ [t|CsrSrc|])
      1
      [ ConstrRepr 'FromRs1 0b1 0b0 []
      , ConstrRepr 'FromUimm 0b1 0b1 []
      ]
  )
  #-}

deriveBitPack [t|CsrSrc|]

{- | The CSR access a @SYSTEM@ instruction's @funct3@ names: where the operand
comes from and what to do with it. 'Nothing' when bits 1-0 are zero, which is
the @funct3@ of the non-CSR system instructions rather than of a CSR access.

>>> parseCsr 0b101  -- csrrwi
Just (FromUimm,ReadWrite)
>>> parseCsr 0b000
Nothing
-}
parseCsr :: BitVector 3 -> Maybe (CsrSrc, CsrOp)
parseCsr f3 = orNothing (slice d1 d0 f3 /= 0) (unpack (slice d2 d2 f3), unpack (slice d1 d0 f3))

{- | The @funct12@ field of a @SYSTEM@ instruction whose @funct3@ is zero, where
it names the operation rather than a CSR.
-}
newtype System12 = System12 (BitVector 12)
  deriving newtype (Eq)

pattern ECALL, EBREAK, MRET :: System12
{- FOURMOLU_DISABLE -}
pattern ECALL  = System12 0b000000000000
pattern EBREAK = System12 0b000000000001
pattern MRET   = System12 0b001100000010
{- FOURMOLU_ENABLE -}

{- | What a @SYSTEM@ instruction asks for. @funct3@ tells a CSR access from the
rest; among the rest, @ECALL@, @EBREAK@ and @MRET@ are implemented, and nothing
else has a constructor here.
-}
data SystemOp
  = SysCsr (CsrSrc, CsrOp)
  | SysEcall
  | SysEbreak
  | SysMret
  deriving (Generic, NFDataX)

{- | The reason a trap was taken. No exception code in use here goes above 15,
so the code is kept narrow and widened only where @mcause@ is read.
-}
data TrapCause
  = TrapCause
  { interrupt :: Bool
  , code :: BitVector 4
  }
  deriving (Generic, NFDataX)

deriveAutoReg ''TrapCause

pattern ILLEGAL_INSTRUCTION, BREAKPOINT, ENVIRONMENT_CALL_FROM_M_MODE :: TrapCause
pattern ILLEGAL_INSTRUCTION = TrapCause False 2
pattern BREAKPOINT = TrapCause False 3
pattern ENVIRONMENT_CALL_FROM_M_MODE = TrapCause False 11
