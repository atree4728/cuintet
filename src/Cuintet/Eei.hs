-- | RISC-V execution environment interface.
module Cuintet.Eei (
  XLen,
  XLenBytes,
  ILen,
  Addr,
  resetVector,
  Inst,
  Sign (..),
  Width (..),
  MemOp (..),
  parseLoad,
  parseStore,
  LaneOffset,
  laneOffset,
  bitOffset,
  sizeBytes,
  aligned,
  laneMask,
  StoreLanes (..),
  LoadShape (..),
  storeLanes,
  loadResult,
  BusReq (..),
  BusResp (..),
  MemDataBytes,
  MemReq,
  MemResp,
  Opcode (LUI, AUIPC, JAL, JALR, BRANCH, LOAD, STORE, OP_IMM, OP_REG, OP_IMM_32, OP_REG_32, MISC_MEM, SYSTEM),
  AluOp (..),
  BranchOp (..),
  parseBranchOp,
  MulOp (..),
  DivOp (..),
  MulDivOp (..),
  CsrAddr (..),
  parseCsrAddr,
  CsrOp (..),
  CsrSrc (..),
  CsrSpec (..),
  parseCsr,
  System12 (System12, ECALL, EBREAK, MRET),
  SystemOp (..),
  instSlice,
  RegFile,
  RegAddr,
  PRegAddr,
  RobAddr,
  NRegs,
  NPRegs,
  NRob,
  StoreQueueAddr,
  NStoreQueue,
  LoadQueueAddr,
  NLoadQueue,
  Mapping (..),
  TrapCause (..),
  pattern INSTRUCTION_ADDRESS_MISALIGNED,
  pattern ILLEGAL_INSTRUCTION,
  pattern BREAKPOINT,
  pattern LOAD_ADDRESS_MISALIGNED,
  pattern STORE_AMO_ADDRESS_MISALIGNED,
  pattern ENVIRONMENT_CALL_FROM_M_MODE,
  misalignedCause,
  FetchWidth,
  DispatchWidth,
  IssueWidth,
  WriteBackWidth,
  CommitWidth,
) where

import Clash.Annotations.BitRepresentation
import Clash.Annotations.BitRepresentation.Deriving
import Clash.Prelude
import Control.Monad (guard)
import Cuintet.Util (orNothing)

-- | The length of integer registers.
type XLen = 64

-- | The maximum width of instructions which the implementation supports.
type ILen = 32

-- | Widths of the buses are counted in bytes.
type XLenBytes = XLen `Div` 8

-- | A physical memory address.
type Addr = Unsigned XLen

-- | Where fetch starts, and where the images are linked.
resetVector :: Addr
resetVector = 0x80000000

-- | An instruction word. RV64I has the 32-bit form only.
type Inst = BitVector ILen

-- | The integer registers.
type RegFile = Vec 32 (BitVector XLen)

-- | A register index.
type RegAddr = Unsigned 5

type PRegAddr = Unsigned 6

type NRegs = 2 ^ BitSize RegAddr

type NPRegs = 2 ^ BitSize PRegAddr

type RobAddr = Unsigned 4

type NRob = 2 ^ BitSize RobAddr

type StoreQueueAddr = Unsigned 4

type NStoreQueue = 2 ^ BitSize StoreQueueAddr

type LoadQueueAddr = Unsigned 4

type NLoadQueue = 2 ^ BitSize LoadQueueAddr

{- | What renaming an instruction's destination register decided: Cm makes it architectural, and
the physical register the architectural map table held until then goes back to the free list.
-}
data Mapping = Mapping
  { rdAddr :: RegAddr
  , pdAddr :: PRegAddr
  }
  deriving (Generic, NFDataX)

-- | Whether a narrower-than-register load fills the high bits with its sign or zero.
data Sign = Signed | Unsigned
  deriving (Generic, NFDataX, Eq, Show)

deriveDefaultAnnotation [t|Sign|]
deriveBitPack [t|Sign|]

-- | The width of a memory access.
data Width = Byte | Half | Word | Double
  deriving (Generic, NFDataX, Show)

deriveDefaultAnnotation [t|Width|]
deriveBitPack [t|Width|]

-- | What a memory instruction asks of memory.
data MemOp = Load Width Sign | Store Width
  deriving (Generic, NFDataX)

-- | The width and sign a load's @funct3@. 'Nothing' for @0b111@.
parseLoad :: BitVector 3 -> Maybe (Width, Sign)
parseLoad f3 = orNothing (f3 /= 0b111) (unpack (slice d1 d0 f3), unpack (slice d2 d2 f3))

-- | The width a store of @funct3 = @0b1xx@.
parseStore :: BitVector 3 -> Maybe Width
parseStore f3 = orNothing (slice d2 d2 f3 == 0) (unpack (slice d1 d0 f3))

-- | The byte offset of an access within its word, the lane 0 being the least significant.
type LaneOffset = Index XLenBytes

-- | The offset of the address within its word.
laneOffset :: Addr -> LaneOffset
laneOffset a = numConvert (truncateB (pack a) :: BitVector (CLog 2 XLenBytes))

-- | The offset in bits, to shift a word into or out of place.
bitOffset :: LaneOffset -> Int
bitOffset off = 8 * numConvert off

-- | The size of the access, in bytes.
sizeBytes :: Width -> Index (XLenBytes + 1)
sizeBytes = \case
  Byte -> 1
  Half -> 2
  Word -> 4
  Double -> 8

-- | Whether the access is naturally aligned.
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

instSlice :: Addr -> BitVector (MemDataBytes * 8) -> Vec FetchWidth (Maybe Inst)
instSlice addr busWord
  | pack addr `testBit` 2 = Just upper :> Nothing :> Nil
  | otherwise = Just lower :> Just upper :> Nil
  where
    (upper, lower) = split busWord

-- | Data to be written, which is masked and divided into bytes.
newtype StoreLanes nBytes = StoreLanes (Vec nBytes (Maybe (BitVector 8)))
  deriving stock (Generic)
  deriving anyclass (NFDataX)
  deriving newtype (Eq)

-- | Load request, which is to be sliced and extended.
data LoadShape = LoadShape {width :: Width, sign :: Sign, offset :: LaneOffset}
  deriving (Generic, NFDataX)

-- | Construct the byte lanes to write.
storeLanes :: Width -> LaneOffset -> BitVector (MemDataBytes * 8) -> StoreLanes MemDataBytes
storeLanes width offset word = StoreLanes $ zipWith orNothing (laneMask width offset) bytes
  where
    bytes = reverse $ bitCoerce $ word `shiftL` bitOffset offset

{- | The value a load produces: the bus word sliced and extended to its 'LoadShape'.

>>> import Clash.Prelude
>>> 0xdeadbeef :: BitVector 64
0b0000_0000_0000_0000_0000_0000_0000_0000_1101_1110_1010_1101_1011_1110_1110_1111
>>> loadResult LoadShape{width = Byte, sign = Signed, offset = 0} 0xdeadbeef   -- lb
0b1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1110_1111
>>> loadResult LoadShape{width = Byte, sign = Unsigned, offset = 1} 0xdeadbeef -- lbu
0b0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_1011_1110
>>> loadResult LoadShape{width = Half, sign = Signed, offset = 2} 0xdeadbeef   -- lh
0b1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1101_1110_1010_1101
>>> loadResult LoadShape{width = Half, sign = Unsigned, offset = 0} 0xdeadbeef -- lhu
0b0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_1011_1110_1110_1111
>>> loadResult LoadShape{width = Word, sign = Signed, offset = 0} 0xdeadbeef   -- lw
0b1111_1111_1111_1111_1111_1111_1111_1111_1101_1110_1010_1101_1011_1110_1110_1111
-}
loadResult :: LoadShape -> BitVector (MemDataBytes * 8) -> BitVector XLen
loadResult LoadShape {width, sign, offset} busWord = case width of
  Byte -> ext sign (truncateB shifted :: BitVector 8)
  Half -> ext sign (truncateB shifted :: BitVector 16)
  Word -> ext sign (truncateB shifted :: BitVector 32)
  Double -> busWord
  where
    shifted = busWord `shiftR` bitOffset offset
    ext Signed = signExtend
    ext Unsigned = zeroExtend

{- | Memory access request, carried on the bus as @Maybe (MemBusReq ...)@;
@Nothing@ means no access.
-}
data BusReq nBytes = BusReq
  { addr :: Addr
  -- ^ The address to access.
  , wdata :: Maybe (StoreLanes nBytes)
  -- ^ 'Just' the data to write for stores, 'Nothing' for loads.
  }
  deriving (Generic, NFDataX, Eq)

-- | The memory's half of the bus: whether it takes a request this cycle, and the word read for one it took earlier.
data BusResp nBytes = BusResp
  { ready :: Bool
  -- ^ Whether to accept a memory access request.
  , rdata :: Maybe (BitVector (nBytes * 8))
  -- ^ Data read.
  }
  deriving (Generic, NFDataX)

-- | The width of the memory bus, in bytes.
type MemDataBytes = XLenBytes

-- | 'BusReq' at the width the memory bus is.
type MemReq = BusReq MemDataBytes

-- | 'BusResp' at the width the memory bus is.
type MemResp = BusResp MemDataBytes

type FetchWidth = MemDataBytes * 8 `Div` ILen

-- | The @opcode@ field.
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

-- | The ALU operation of an @OP@ or @OP-IMM@ instruction, derived from @unpack (funct3 ++# inst[30])@.
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
      [ ConstrRepr 'ADD 0b1111 0b0000 []
      , ConstrRepr 'SUB 0b1111 0b0001 []
      , ConstrRepr 'SLL 0b1111 0b0010 []
      , ConstrRepr 'SLT 0b1111 0b0100 []
      , ConstrRepr 'SLTU 0b1111 0b0110 []
      , ConstrRepr 'XOR 0b1111 0b1000 []
      , ConstrRepr 'SRL 0b1111 0b1010 []
      , ConstrRepr 'SRA 0b1111 0b1011 []
      , ConstrRepr 'OR 0b1111 0b1100 []
      , ConstrRepr 'AND 0b1111 0b1110 []
      ]
  )
  #-}

deriveBitPack [t|AluOp|]

-- | The branch condition, derived from @unpack funct3@.
data BranchOp
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
      $(liftQ [t|BranchOp|])
      3
      [ ConstrRepr 'BEQ 0b111 0b000 []
      , ConstrRepr 'BNE 0b111 0b001 []
      , ConstrRepr 'BLT 0b111 0b100 []
      , ConstrRepr 'BGE 0b111 0b101 []
      , ConstrRepr 'BLTU 0b111 0b110 []
      , ConstrRepr 'BGEU 0b111 0b111 []
      ]
  )
  #-}

deriveBitPack [t|BranchOp|]

-- | The branch a @funct3@ names, or 'Nothing' for the two patterns that name none: @0b01x@.
parseBranchOp :: BitVector 3 -> Maybe BranchOp
parseBranchOp f3 = orNothing (slice d2 d1 f3 /= 0b01) (unpack f3)

data MulOp = MulLow | MulHighHom Sign | MulHighHetero
  deriving (Eq, Generic, NFDataX)

data DivOp = Div Sign | Rem Sign
  deriving (Eq, Generic, NFDataX)

data MulDivOp = Multiply MulOp | Division DivOp
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
      $(liftQ [t|MulDivOp|])
      3
      [ ConstrRepr 'Multiply 0b100 0b000 [0b011]
      , ConstrRepr 'Division 0b100 0b100 [0b011]
      ]
  )
  #-}

deriveBitPack [t|MulDivOp|]

data CsrAddr = MSTATUS | MIE | MTVEC | MEPC | MCAUSE | MTVAL | LED | MCYCLE | MHARTID
  deriving (Generic, NFDataX, Eq, Show)

parseCsrAddr :: BitVector 12 -> Maybe CsrAddr
parseCsrAddr 0x300 = Just MSTATUS
parseCsrAddr 0x304 = Just MIE
parseCsrAddr 0x305 = Just MTVEC
parseCsrAddr 0x341 = Just MEPC
parseCsrAddr 0x342 = Just MCAUSE
parseCsrAddr 0x343 = Just MTVAL
parseCsrAddr 0x800 = Just LED
parseCsrAddr 0xB00 = Just MCYCLE
parseCsrAddr 0xF14 = Just MHARTID
parseCsrAddr _ = Nothing

-- | What a CSR access does to the register, derived from @funct3[1:0]@.
data CsrOp
  = ReadWrite
  | ReadSet
  | ReadClear
  deriving (Generic, NFDataX, Eq, Show)

{-# ANN
  module
  ( DataReprAnn
      $(liftQ [t|CsrOp|])
      2
      [ ConstrRepr 'ReadWrite 0b11 0b01 []
      , ConstrRepr 'ReadSet 0b11 0b10 []
      , ConstrRepr 'ReadClear 0b11 0b11 []
      ]
  )
  #-}

deriveBitPack [t|CsrOp|]

data CsrSrc = Rs1 | Uimm (BitVector 5)
  deriving (Generic, NFDataX, Eq, Show)

data CsrSpec = CsrSpec
  { csrAddr :: CsrAddr
  , csrOp :: CsrOp
  , csrSrc :: Maybe CsrSrc
  }
  deriving (Generic, NFDataX, Eq)

parseCsr :: BitVector 3 -> BitVector 12 -> BitVector 5 -> Maybe CsrSpec
parseCsr f3 f12 rs1Addr = do
  guard $ slice d1 d0 f3 /= 0
  csrAddr <- parseCsrAddr f12
  pure CsrSpec {..}
  where
    csrOp = unpack (slice d1 d0 f3)
    src
      | f3 `testBit` 2 = Uimm rs1Addr
      | otherwise = Rs1
    csrSrc = orNothing (writes csrOp rs1Addr) src
    writes ReadSet 0 = False
    writes ReadClear 0 = False
    writes _ _ = True

-- | The @funct12@ field of a @SYSTEM@ instruction whose @funct3@ is zero.
newtype System12 = System12 (BitVector 12)
  deriving newtype (Eq)

pattern ECALL, EBREAK, MRET :: System12
{- FOURMOLU_DISABLE -}
pattern ECALL  = System12 0b000000000000
pattern EBREAK = System12 0b000000000001
pattern MRET   = System12 0b001100000010
{- FOURMOLU_ENABLE -}

-- | What a @SYSTEM@ instruction asks for.
data SystemOp
  = SysCsr CsrSpec
  | SysEcall
  | SysEbreak
  | SysMret
  deriving (Generic, NFDataX, Eq)

-- | The reason a trap was taken.
data TrapCause
  = TrapCause
  { interrupt :: Bool
  , code :: BitVector 4
  }
  deriving (Generic, NFDataX, Eq)

deriveAutoReg ''TrapCause

pattern INSTRUCTION_ADDRESS_MISALIGNED, ILLEGAL_INSTRUCTION, BREAKPOINT, LOAD_ADDRESS_MISALIGNED, STORE_AMO_ADDRESS_MISALIGNED, ENVIRONMENT_CALL_FROM_M_MODE :: TrapCause
pattern INSTRUCTION_ADDRESS_MISALIGNED = TrapCause False 0
pattern ILLEGAL_INSTRUCTION = TrapCause False 2
pattern BREAKPOINT = TrapCause False 3
pattern LOAD_ADDRESS_MISALIGNED = TrapCause False 4
pattern STORE_AMO_ADDRESS_MISALIGNED = TrapCause False 6
pattern ENVIRONMENT_CALL_FROM_M_MODE = TrapCause False 11

misalignedCause :: MemOp -> Addr -> Maybe TrapCause
misalignedCause memOp addr = orNothing (not $ aligned width $ laneOffset addr) cause
  where
    (width, cause) = case memOp of
      Load w _ -> (w, LOAD_ADDRESS_MISALIGNED)
      Store w -> (w, STORE_AMO_ADDRESS_MISALIGNED)

type DispatchWidth = 2

type IssueWidth = 2

type WriteBackWidth = 2

type CommitWidth = 2
