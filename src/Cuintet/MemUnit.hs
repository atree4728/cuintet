{- |
Memory unit: executes load\/store instructions by sending the address
computed by the ALU to the memory bus.

An access takes at least 3 cycles ('Init' → 'WaitReady' → 'WaitValid'),
during which the core must stall (no write back, no instruction fetch).

The memory ignores the low 2 bits of the address, so it always returns the
word containing the target. Sub-word loads (LB\/LH\/LBU\/LHU) select the bytes
from that word by 'formatRdata'; sub-word stores (SB\/SH) need a byte enable on
the bus and are not supported yet. Accesses that straddle a word boundary
(e.g. LH at an odd address) are rejected as 'deepErrorX'.
-}
module Cuintet.MemUnit (
  DataWidth (..),
  InstInfo (..),
  LoadFmt (..),
  MemUnitReq (..),
  MemUnitResp (..),
  MemUnitState (..),
  formatRdata,
  memUnitStep,
  memUnit,
) where

import Clash.Prelude
import Cuintet.Corectrl (InstCtrl (..), isMemOp, isStoreOp)
import Cuintet.Eei (Addr, MemBusReq (..), MemBusResp (..), MemDataWidth, XLen)
import Cuintet.Util (fill)
import Data.Maybe (isJust, isNothing)

-- | The instruction supplied to the memory unit.
data InstInfo = InstInfo
  { isNew :: Bool
  -- ^ Whether the instruction is newly supplied this cycle.
  , ctrl :: InstCtrl
  , addr :: Addr
  -- ^ The access address computed by the ALU.
  , rs2 :: BitVector XLen
  -- ^ Data to store.
  }
  deriving (Generic, NFDataX)

data MemUnitReq = MemUnitReq
  { inst :: Maybe InstInfo
  -- ^ The instruction being executed, if any.
  , memresp :: MemBusResp XLen
  }
  deriving (Generic, NFDataX)

data MemUnitResp = MemUnitResp
  { rdata :: Maybe (BitVector XLen)
  , stall :: Bool
  -- ^ Whether the core must stall for an access in flight
  , memreq :: Maybe (MemBusReq MemDataWidth XLen)
  }
  deriving (Generic, NFDataX)

data DataWidth
  = -- | Byte i -> [(i+1)*8-1:i*8]
    Byte (BitVector 2)
  | -- | Half i -> [(i+1)*16-1:i*16]
    Half (BitVector 1)
  | -- | Word   -> [31:0]
    Word
  deriving (Generic, NFDataX)

data LoadFmt = LoadFmt
  { width :: DataWidth
  , signed :: Bool
  }
  deriving (Generic, NFDataX)

data Access
  = Store (BitVector MemDataWidth)
  | Load LoadFmt
  deriving (Generic, NFDataX)

data MemUnitState
  = -- | Wait for a new memory instruction; latch its request and move to 'WaitReady'.
    Init
  | -- | Keep sending the request until the memory accepts it, then move to 'WaitValid', with @(addr, wdata)@
    WaitReady Addr Access
  | -- | Wait until the access completes, then move back to 'Init'.
    WaitValid Access
  deriving (Generic, NFDataX)

-- | The state transision of the memory unit for a single cycle.
memUnitStep :: MemUnitState -> MemUnitReq -> (MemUnitState, MemUnitResp)
memUnitStep state MemUnitReq{inst, memresp} = (memUnitState, memUnitResp)
 where
  isNewMemOp InstInfo{isNew, ctrl} = isNew && isMemOp ctrl
  memUnitState = case state of
    Init | Just i <- inst, isNewMemOp i, isStoreOp i.ctrl -> WaitReady i.addr (Store i.rs2)
    Init | Just i <- inst, isNewMemOp i -> WaitReady i.addr (Load $ loadFmt i.ctrl.funct3 i.addr)
    WaitReady _ acc | memresp.ready -> WaitValid acc
    WaitValid _ | isJust memresp.rdata -> Init
    _ -> state
  memUnitResp =
    MemUnitResp
      { rdata = case state of
          WaitValid (Load fmt) -> formatRdata fmt <$> memresp.rdata
          _ -> Nothing
      , -- in 'Init' when a new memory instruction arrives,
        -- in 'WaitReady' always,
        -- in 'WaitValid' until the response arrives.
        stall = case (inst, state) of
          (Nothing, _) -> False
          (Just i, Init) -> isNewMemOp i
          (Just _, WaitReady _ _) -> True
          (Just _, WaitValid _) -> isNothing memresp.rdata
      , memreq = case state of
          WaitReady reqAddr (Load _) -> Just MemBusReq{addr = reqAddr, wdata = Nothing}
          WaitReady reqAddr (Store wdata) -> Just MemBusReq{addr = reqAddr, wdata = Just wdata}
          _ -> Nothing
      }

-- | Construct a load format from @funct3@ and the access address.
loadFmt :: BitVector 3 -> Addr -> LoadFmt
loadFmt funct3 addr = LoadFmt{width, signed}
 where
  signed = not (funct3 `testBit` 2)
  offset :: BitVector 2
  offset = truncateB (pack addr)
  width = case slice d1 d0 funct3 of
    0b00 -> Byte offset
    0b01 | not (offset `testBit` 0) -> Half (slice d1 d1 offset)
    0b10 -> Word
    _ -> deepErrorX "loadFmt: invalid funct3"

{- | Format loaded word according to 'LoadFmt'.

>>> import Clash.Prelude
>>> 0xdeadbeef :: BitVector 32
0b1101_1110_1010_1101_1011_1110_1110_1111
>>> formatRdata LoadFmt{width = Byte 0, signed = True} 0xdeadbeef  -- lb
0b1111_1111_1111_1111_1111_1111_1110_1111
>>> formatRdata LoadFmt{width = Byte 1, signed = False} 0xdeadbeef -- lbu
0b0000_0000_0000_0000_0000_0000_1011_1110
>>> formatRdata LoadFmt{width = Half 1, signed = True} 0xdeadbeef  -- lh
0b1111_1111_1111_1111_1101_1110_1010_1101
>>> formatRdata LoadFmt{width = Half 0, signed = False} 0xdeadbeef -- lhu
0b0000_0000_0000_0000_1011_1110_1110_1111
>>> formatRdata LoadFmt{width = Word, signed = True} 0xdeadbeef    -- lw
0b1101_1110_1010_1101_1011_1110_1110_1111
-}
formatRdata :: LoadFmt -> BitVector XLen -> BitVector XLen
formatRdata LoadFmt{width, signed} word = case width of
  Byte offset -> ext (sliceByte offset word :: BitVector 8)
  Half offset -> ext (sliceHalf offset word :: BitVector 16)
  Word -> word
 where
  sliceByte offset d = truncateB $ d `shiftR` (8 * numConvert offset)
  sliceHalf offset d = truncateB $ d `shiftR` (16 * numConvert offset)
  ext :: (KnownNat n, KnownNat m) => BitVector n -> BitVector (n + m)
  ext v = fill (if signed then msb v else low) ++# v

{- | A then wrapper for unit tests.

1 回の load を 'Init' → 'WaitReady' → 'WaitValid' → 'Init' と一巡させる例:

>>> import Prelude
>>> import Clash.Prelude
>>> import Cuintet.Corectrl (InstCtrl (..), InstType (..))
>>> import Cuintet.Eei (MemBusReq (..), MemBusResp (..))
>>> ctrl = InstCtrl{itype = IType, rwbEn = True, isLui = False, isAluOp = False, isJump = False, isLoad = True, funct3 = 0b010, funct7 = 0}
>>> info isNew = InstInfo{isNew, ctrl, addr = 100, rs2 = 0}
>>> mkReq inst ready rdata = MemUnitReq{inst, memresp = MemBusResp{ready, rdata}}
>>> reqs = [mkReq (Just (info True)) False Nothing, mkReq (Just (info False)) True Nothing, mkReq (Just (info False)) True Nothing, mkReq (Just (info False)) True (Just 0xdeadbeef), mkReq Nothing True Nothing]
>>> resps = simulateN @System (Prelude.length reqs) memUnit reqs
>>> (.stall) <$> resps
[True,True,True,False,False]
>>> (fmap (\r -> (r.addr, r.wdata)) . (.memreq)) <$> resps
[Nothing,Just (100,Nothing),Nothing,Nothing,Nothing]
>>> (.rdata) <$> resps
[Nothing,Nothing,Nothing,Just 0b1101_1110_1010_1101_1011_1110_1110_1111,Nothing]
-}
memUnit :: (HiddenClockResetEnable dom) => Signal dom MemUnitReq -> Signal dom MemUnitResp
memUnit = mealy memUnitStep Init
