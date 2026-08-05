{- |
Memory unit: executes load\/store instructions by sending the address
computed by the ALU to the memory bus.

An access takes at least 3 cycles ('Idle' → 'WaitReady' → 'WaitValid'),
during which the core must stall (no write back, no instruction fetch).

The memory ignores the low 2 bits of the address, so it always returns the
word containing the target. Sub-word loads (LB\/LH\/LBU\/LHU) select the bytes
from that word by 'formatRdata'; sub-word stores (SB\/SH) mask off the byte
lanes outside the access by 'storeLanes'. Accesses that straddle a word boundary
(e.g. LH at an odd address) are rejected as 'deepErrorX'.
-}
module Cuintet.MemUnit (
  ByteRange (..),
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
import Cuintet.CoreCtrl (InstCtrl (..), isMemOp, isStore)
import Cuintet.Eei (Addr, ByteRange (..), DataResp, LoadFmt (..), MemBusReq (..), MemBusResp (..), MemDataBytes, StoreLanes (..), XLen, XLenBytes, bitOffset, laneMask)
import Cuintet.Util (orNothing)
import Data.Maybe (isJust, isNothing)

-- | The instruction supplied to the memory unit.
data InstInfo = InstInfo
  { isNew :: Bool
  -- ^ Whether the instruction is newly supplied this cycle.
  , ctrl :: InstCtrl
  , addr :: Addr
  -- ^ The access address computed by the ALU.
  , wdata :: BitVector XLen
  -- ^ Data to store.
  }
  deriving (Generic, NFDataX)

data MemUnitReq = MemUnitReq
  { inst :: Maybe InstInfo
  -- ^ The instruction being executed, if any.
  , memResp :: DataResp
  }
  deriving (Generic, NFDataX)

data MemUnitResp = MemUnitResp
  { rdata :: Maybe (BitVector XLen)
  , stall :: Bool
  -- ^ Whether the core must stall for an access in flight
  , memReq :: Maybe (MemBusReq MemDataBytes XLen)
  }
  deriving (Generic, NFDataX)

data Access
  = Store (StoreLanes MemDataBytes)
  | Load LoadFmt
  deriving (Generic, NFDataX)

data MemUnitState
  = -- | Wait for a new memory instruction; latch its request and move to 'WaitReady'.
    Idle
  | -- | Keep sending the request until the memory accepts it, then move to 'WaitValid', with @(addr, wdata)@
    WaitReady Addr Access
  | -- | Wait until the access completes, then move back to 'Idle'.
    WaitValid Access
  deriving (Generic, NFDataX)

-- | The state transision of the memory unit for a single cycle.
memUnitStep :: MemUnitState -> MemUnitReq -> (MemUnitState, MemUnitResp)
memUnitStep state MemUnitReq{inst, memResp} = (memUnitState, memUnitResp)
 where
  isNewMemOp InstInfo{isNew, ctrl} = isNew && isMemOp ctrl
  memUnitState = case state of
    Idle | Just i <- inst, isNewMemOp i, isStore i.ctrl -> WaitReady i.addr (Store $ storeLanes i.ctrl.funct3 i.addr i.wdata)
    Idle | Just i <- inst, isNewMemOp i -> WaitReady i.addr (Load $ loadFmt i.ctrl.funct3 i.addr)
    WaitReady _ acc | memResp.ready -> WaitValid acc
    WaitValid _ | isJust memResp.rdata -> Idle
    _ -> state
  memUnitResp =
    MemUnitResp
      { rdata = case state of
          WaitValid (Load fmt) -> formatRdata fmt <$> memResp.rdata
          _ -> Nothing
      , -- in 'Idle' when a new memory instruction arrives,
        -- in 'WaitReady' always,
        -- in 'WaitValid' until the response arrives.
        stall = case (inst, state) of
          (Nothing, _) -> False
          (Just i, Idle) -> isNewMemOp i
          (Just _, WaitReady _ _) -> True
          (Just _, WaitValid _) -> isNothing memResp.rdata
      , memReq = case state of
          WaitReady reqAddr (Load _) -> Just MemBusReq{addr = reqAddr, wdata = Nothing}
          WaitReady reqAddr (Store wdata) -> Just MemBusReq{addr = reqAddr, wdata = Just wdata}
          _ -> Nothing
      }

accessRange :: BitVector 3 -> Addr -> ByteRange
accessRange funct3 addr = case slice d1 d0 funct3 of
  0b00 -> Byte $ numConvert offset
  0b01 | not (offset `testBit` 0) -> Half $ numConvert $ slice d1 d1 offset
  0b10 -> Word
  _ -> deepErrorX "accessRange: invalid funct3"
 where
  offset :: BitVector (CLog 2 XLenBytes)
  offset = truncateB (pack addr)

-- | Construct a load format from @funct3@ and the access address.
loadFmt :: BitVector 3 -> Addr -> LoadFmt
loadFmt funct3 addr = LoadFmt{range, signed}
 where
  signed = not (funct3 `testBit` 2)
  range = accessRange funct3 addr

{- | Construct the byte lanes to write from @funct3@ and the access address and written word.

The word is shifted into place by @8 * byteOffset@ bits, and the lanes it
occupies are given by the same offset in bytes.
-}
storeLanes :: BitVector 3 -> Addr -> BitVector XLen -> StoreLanes MemDataBytes
storeLanes funct3 addr word = StoreLanes $ zipWith orNothing (laneMask range) bytes
 where
  range = accessRange funct3 addr
  bytes = reverse $ bitCoerce $ word `shiftL` bitOffset range

{- | Format loaded word according to 'LoadFmt'.

>>> import Clash.Prelude
>>> 0xdeadbeef :: BitVector 32
0b1101_1110_1010_1101_1011_1110_1110_1111
>>> formatRdata LoadFmt{range = Byte 0, signed = True} 0xdeadbeef  -- lb
0b1111_1111_1111_1111_1111_1111_1110_1111
>>> formatRdata LoadFmt{range = Byte 1, signed = False} 0xdeadbeef -- lbu
0b0000_0000_0000_0000_0000_0000_1011_1110
>>> formatRdata LoadFmt{range = Half 1, signed = True} 0xdeadbeef  -- lh
0b1111_1111_1111_1111_1101_1110_1010_1101
>>> formatRdata LoadFmt{range = Half 0, signed = False} 0xdeadbeef -- lhu
0b0000_0000_0000_0000_1011_1110_1110_1111
>>> formatRdata LoadFmt{range = Word, signed = True} 0xdeadbeef    -- lw
0b1101_1110_1010_1101_1011_1110_1110_1111
-}
formatRdata :: LoadFmt -> BitVector XLen -> BitVector XLen
formatRdata LoadFmt{range, signed} word = case range of
  Byte _ -> ext (truncateB shifted :: BitVector 8)
  Half _ -> ext (truncateB shifted :: BitVector 16)
  Word -> word
 where
  shifted = word `shiftR` bitOffset range
  ext :: (KnownNat n, KnownNat m) => BitVector n -> BitVector (m + n)
  ext = if signed then signExtend else zeroExtend

-- | A thin wrapper for unit tests.
memUnit :: (HiddenClockResetEnable dom) => Signal dom MemUnitReq -> Signal dom MemUnitResp
memUnit = mealy memUnitStep Idle
