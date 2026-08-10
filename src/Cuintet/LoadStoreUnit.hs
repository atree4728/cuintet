{- |
Load\/store unit: executes load\/store instructions by sending the address
computed by the ALU to the memory bus.

An access takes at least 3 cycles ('Idle' → 'WaitReady' → 'WaitValid'), during
which it holds MA with @stall@. The stages above keep running until the FIFOs
between them fill up.

The memory is addressed in bus words of 'MemDataBytes' bytes and ignores the
offset within one, so it always returns the bus word containing the target.
Narrower loads (LB\/LH\/LW and their unsigned forms) select the bytes from that
word by 'formatRdata'; narrower stores (SB\/SH\/SW) mask off the byte lanes
outside the access by 'storeLanes'. Accesses that are not naturally aligned are
rejected as 'deepErrorX' by 'access', so one never straddles two bus words.
-}
module Cuintet.LoadStoreUnit (
  AccessWidth (..),
  InstInfo (..),
  LoadFmt (..),
  Sign (..),
  LoadStoreReq (..),
  LoadStoreResp (..),
  LoadStoreState (..),
  loadStoreStep,
  formatRdata,
) where

import Clash.Prelude
import Cuintet.CoreCtrl (InstCtrl (..), isMemOp, isStore)
import Cuintet.Eei (AccessWidth (..), Addr, BusReq (..), BusResp (..), LaneOffset, LoadFmt (..), MemDataBytes, MemReq, MemResp, Sign (..), StoreLanes (..), XLen, aligned, bitOffset, laneMask, laneOffset)
import Cuintet.Util (orNothing)
import Data.Maybe (isJust, isNothing)

-- | The instruction supplied to the load\/store unit.
data InstInfo = InstInfo
  { ctrl :: InstCtrl
  , addr :: Addr
  -- ^ The access address computed by the ALU.
  , wdata :: BitVector XLen
  -- ^ Data to store.
  }
  deriving (Generic, NFDataX)

data LoadStoreReq = LoadStoreReq
  { inst :: Maybe InstInfo
  -- ^ The instruction being executed, if any.
  , memResp :: MemResp
  }
  deriving (Generic, NFDataX)

data LoadStoreResp = LoadStoreResp
  { rdata :: Maybe (BitVector XLen)
  , stall :: Bool
  -- ^ Whether the core must stall for an access in flight
  , memReq :: Maybe MemReq
  }
  deriving (Generic, NFDataX)

data Access
  = Store (StoreLanes MemDataBytes)
  | Load LoadFmt
  deriving (Generic, NFDataX)

data LoadStoreState
  = -- | Wait for a new memory instruction; latch its request and move to 'WaitReady'.
    Idle
  | -- | Keep sending the request until the memory accepts it, then move to 'WaitValid', with @(addr, wdata)@
    WaitReady Addr Access
  | -- | Wait until the access completes, then move back to 'Idle'.
    WaitValid Access
  deriving (Generic, NFDataX)

-- | One cycle of the load\/store unit.
loadStoreStep :: LoadStoreState -> LoadStoreReq -> (LoadStoreState, LoadStoreResp)
loadStoreStep state LoadStoreReq {inst, memResp} = (memUnitState, memUnitResp)
  where
    memUnitState = case state of
      Idle | Just i <- inst, isMemOp i.ctrl -> WaitReady i.addr (access i.ctrl i.addr i.wdata)
      WaitReady _ acc | memResp.ready -> WaitValid acc
      WaitValid _ | isJust memResp.rdata -> Idle
      _ -> state
    memUnitResp =
      LoadStoreResp
        { rdata = case state of
            WaitValid (Load fmt) -> formatRdata fmt <$> memResp.rdata
            _ -> Nothing
        , -- in 'Idle' when a new memory instruction arrives,
          -- in 'WaitReady' always,
          -- in 'WaitValid' until the response arrives.
          stall = case (inst, state) of
            (Nothing, _) -> False
            (Just i, Idle) -> isMemOp i.ctrl
            (Just _, WaitReady _ _) -> True
            (Just _, WaitValid _) -> isNothing memResp.rdata
        , memReq = case state of
            WaitReady reqAddr (Load _) -> Just BusReq {addr = reqAddr, wdata = Nothing}
            WaitReady reqAddr (Store wdata) -> Just BusReq {addr = reqAddr, wdata = Just wdata}
            _ -> Nothing
        }

{- | The access an instruction requests. The width is @funct3@ read directly; the offset comes from the address.

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
>>> formatRdata LoadFmt{width = Word Signed, offset = 0} 0xdeadbeef   -- lw
0b1111_1111_1111_1111_1111_1111_1111_1111_1101_1110_1010_1101_1011_1110_1110_1111

The width is the @funct3@ field verbatim:

>>> (unpack 0b001 :: AccessWidth, unpack 0b101 :: AccessWidth)  -- lh, lhu
(Half Signed,Half Unsigned)
-}
formatRdata :: LoadFmt -> BitVector (MemDataBytes * 8) -> BitVector XLen
formatRdata LoadFmt {width, offset} busWord = case width of
  Byte sign -> ext sign (truncateB shifted :: BitVector 8)
  Half sign -> ext sign (truncateB shifted :: BitVector 16)
  Word sign -> ext sign (truncateB shifted :: BitVector 32)
  DoubleWord -> busWord
  WidthIllegal -> deepErrorX "formatRdata: illegal access width"
  where
    shifted = busWord `shiftR` bitOffset offset
    ext Signed = signExtend
    ext Unsigned = zeroExtend
