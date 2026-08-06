{- |
Memory unit: executes load\/store instructions by sending the address
computed by the ALU to the memory bus.

An access takes at least 3 cycles ('Idle' → 'WaitReady' → 'WaitValid'),
during which the core must stall (no write back, no instruction fetch).

The memory ignores the low 2 bits of the address, so it always returns the
word containing the target. Sub-word loads (LB\/LH\/LBU\/LHU) select the bytes
from that word by 'formatRdata'; sub-word stores (SB\/SH) mask off the byte
lanes outside the access by 'storeLanes'. Accesses that straddle a word boundary
(e.g. LH at an odd address) are rejected as 'deepErrorX' by 'access'.
-}
module Cuintet.MemUnit (
  AccessWidth (..),
  InstInfo (..),
  LoadFmt (..),
  Sign (..),
  MemUnitReq (..),
  MemUnitResp (..),
  MemUnitState (..),
  formatRdata,
  memUnitStep,
  memUnit,
) where

import Clash.Prelude
import Cuintet.CoreCtrl (InstCtrl (..), isMemOp, isStore)
import Cuintet.Eei (AccessWidth (..), Addr, DataResp, LaneOffset, LoadFmt (..), MemBusReq (..), MemBusResp (..), MemDataBytes, Sign (..), StoreLanes (..), XLen, aligned, bitOffset, laneMask, laneOffset)
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
memUnitStep state MemUnitReq {inst, memResp} = (memUnitState, memUnitResp)
  where
    isNewMemOp InstInfo {isNew, ctrl} = isNew && isMemOp ctrl
    memUnitState = case state of
      Idle | Just i <- inst, isNewMemOp i -> WaitReady i.addr (access i.ctrl i.addr (truncateB i.wdata)) -- for now
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
            WaitReady reqAddr (Load _) -> Just MemBusReq {addr = reqAddr, wdata = Nothing}
            WaitReady reqAddr (Store wdata) -> Just MemBusReq {addr = reqAddr, wdata = Just wdata}
            _ -> Nothing
        }

{- | The access an instruction requests. The width is @funct3@ read directly;
the offset comes from the address.

A misaligned access traps in RISC-V, but there is no trap mechanism yet, so it
is rejected as 'deepErrorX'. An illegal @funct3@ is rejected the same way, by
'sizeBytes' inside 'aligned'.
-}
access :: InstCtrl -> Addr -> BitVector (MemDataBytes * 8) -> Access
access ctrl addr wdata
  | not (aligned width offset) = deepErrorX "access: misaligned access"
  | isStore ctrl = Store (storeLanes width offset wdata)
  | otherwise = Load LoadFmt {width, offset}
  where
    width = unpack ctrl.funct3
    offset = laneOffset addr

{- | Construct the byte lanes to write.

The word is shifted into place by @8 * offset@ bits, and the lanes it occupies
are given by the same offset in bytes.
-}
storeLanes :: AccessWidth -> LaneOffset -> BitVector (MemDataBytes * 8) -> StoreLanes MemDataBytes
storeLanes width offset word = StoreLanes $ zipWith orNothing (laneMask width offset) bytes
  where
    bytes = reverse $ bitCoerce $ word `shiftL` bitOffset offset

{- | Format loaded word according to 'LoadFmt'.

>>> import Clash.Prelude
>>> 0xdeadbeef :: BitVector 64
0b0000_0000_0000_0000_0000_0000_0000_0000_1101_1110_1010_1101_1011_1110_1110_1111
>>> formatRdata LoadFmt{width = Byte Signed, offset = 0} 0xdeadbeef   -- lb
0b1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1110_1111
>>> formatRdata LoadFmt{width = Byte Unsigned, offset = 1} 0xdeadbeef -- lbu
0b0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_1011_1110
>>> formatRdata LoadFmt{width = Half Signed, offset = 2} 0xdeadbeef   -- lh
0b1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1101_1110_1010_1101
>>> formatRdata LoadFmt{width = Half Unsigned, offset = 0} 0xdeadbeef -- lhu
0b0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_1011_1110_1110_1111
>>> formatRdata LoadFmt{width = Word, offset = 0} 0xdeadbeef          -- lw
0b1111_1111_1111_1111_1111_1111_1111_1111_1101_1110_1010_1101_1011_1110_1110_1111

The width is the @funct3@ field verbatim:

>>> (unpack 0b001 :: AccessWidth, unpack 0b101 :: AccessWidth)  -- lh, lhu
(Half Signed,Half Unsigned)
-}
formatRdata :: LoadFmt -> BitVector (MemDataBytes * 8) -> BitVector XLen
formatRdata LoadFmt {width, offset} word = case width of
  Byte sign -> ext sign (truncateB shifted :: BitVector 8)
  Half sign -> ext sign (truncateB shifted :: BitVector 16)
  Word -> ext Signed word
  WidthIllegal1 -> illegal
  WidthIllegal2 -> illegal
  WidthIllegal3 -> illegal
  where
    shifted = word `shiftR` bitOffset offset
    illegal = deepErrorX "formatRdata: illegal access width"
    ext Signed = signExtend
    ext Unsigned = zeroExtend

-- | A thin wrapper for unit tests.
memUnit :: (HiddenClockResetEnable dom) => Signal dom MemUnitReq -> Signal dom MemUnitResp
memUnit = mealy memUnitStep Idle
