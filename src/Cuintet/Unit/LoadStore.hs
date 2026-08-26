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
rejected as 'deepErrorX' by 'busAccess', so one never straddles two bus words.
-}
module Cuintet.Unit.LoadStore (
  InstInfo (..),
  LoadFmt (..),
  Width (..),
  Sign (..),
  LoadStoreReq (..),
  LoadStoreResp (..),
  LoadStoreState (..),
  loadStoreStep,
  formatRdata,
) where

import Clash.Prelude
import Cuintet.CoreCtrl (InstCtrl (..), isMemOp)
import Cuintet.Eei (Access (..), Addr, BusReq (..), BusResp (..), LaneOffset, LoadFmt (..), MemDataBytes, MemReq, MemResp, Sign (..), StoreLanes (..), Width (..), XLen, aligned, bitOffset, laneMask, laneOffset)
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

-- | An access as the bus needs it: the lanes to write, or how to format the word that comes back.
data BusAccess
  = BusStore (StoreLanes MemDataBytes)
  | BusLoad LoadFmt
  deriving (Generic, NFDataX)

data LoadStoreState
  = -- | Wait for a new memory instruction; latch its request and move to 'WaitReady'.
    Idle
  | -- | Keep sending the request until the memory accepts it, then move to 'WaitValid', with @(addr, wdata)@
    WaitReady Addr BusAccess
  | -- | Wait until the access completes, then move back to 'Idle'.
    WaitValid BusAccess
  deriving (Generic, NFDataX)

-- | One cycle of the load\/store unit.
loadStoreStep :: LoadStoreState -> LoadStoreReq -> (LoadStoreState, LoadStoreResp)
loadStoreStep state LoadStoreReq {inst, memResp} = (memUnitState, memUnitResp)
  where
    memUnitState = case state of
      Idle | Just i <- inst, Just acc <- i.ctrl.access -> WaitReady i.addr (busAccess acc i.addr i.wdata)
      WaitReady _ acc | memResp.ready -> WaitValid acc
      WaitValid _ | isJust memResp.rdata -> Idle
      _ -> state
    memUnitResp =
      LoadStoreResp
        { rdata = case state of
            WaitValid (BusLoad fmt) -> formatRdata fmt <$> memResp.rdata
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
            WaitReady reqAddr (BusLoad _) -> Just BusReq {addr = reqAddr, wdata = Nothing}
            WaitReady reqAddr (BusStore wdata) -> Just BusReq {addr = reqAddr, wdata = Just wdata}
            _ -> Nothing
        }

{- | The decoded access put in the form the bus needs; the offset is the one part of it the address supplies.

A misaligned access traps in RISC-V, but there is no trap mechanism yet, so it
is rejected as 'deepErrorX'.
-}
busAccess :: Access -> Addr -> BitVector (MemDataBytes * 8) -> BusAccess
busAccess acc addr wdata = case acc of
  Store width -> checked width $ BusStore (storeLanes width offset wdata)
  Load width sign -> checked width $ BusLoad LoadFmt {width, sign, offset}
  where
    offset = laneOffset addr
    checked width x
      | aligned width offset = x
      | otherwise = deepErrorX "busAccess: misaligned access"

{- | Construct the byte lanes to write.

The word is shifted into place by @8 * offset@ bits, and the lanes it occupies
are given by the same offset in bytes.
-}
storeLanes :: Width -> LaneOffset -> BitVector (MemDataBytes * 8) -> StoreLanes MemDataBytes
storeLanes width offset word = StoreLanes $ zipWith orNothing (laneMask width offset) bytes
  where
    bytes = reverse $ bitCoerce $ word `shiftL` bitOffset offset

{- | Format loaded word according to 'LoadFmt'.

>>> import Clash.Prelude
>>> 0xdeadbeef :: BitVector 64
0b0000_0000_0000_0000_0000_0000_0000_0000_1101_1110_1010_1101_1011_1110_1110_1111
>>> formatRdata LoadFmt{width = B, sign = Signed, offset = 0} 0xdeadbeef   -- lb
0b1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1110_1111
>>> formatRdata LoadFmt{width = B, sign = Unsigned, offset = 1} 0xdeadbeef -- lbu
0b0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_1011_1110
>>> formatRdata LoadFmt{width = H, sign = Signed, offset = 2} 0xdeadbeef   -- lh
0b1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1101_1110_1010_1101
>>> formatRdata LoadFmt{width = H, sign = Unsigned, offset = 0} 0xdeadbeef -- lhu
0b0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_1011_1110_1110_1111
>>> formatRdata LoadFmt{width = W, sign = Signed, offset = 0} 0xdeadbeef   -- lw
0b1111_1111_1111_1111_1111_1111_1111_1111_1101_1110_1010_1101_1011_1110_1110_1111
-}
formatRdata :: LoadFmt -> BitVector (MemDataBytes * 8) -> BitVector XLen
formatRdata LoadFmt {width, sign, offset} busWord = case width of
  B -> ext sign (truncateB shifted :: BitVector 8)
  H -> ext sign (truncateB shifted :: BitVector 16)
  W -> ext sign (truncateB shifted :: BitVector 32)
  D -> busWord
  where
    shifted = busWord `shiftR` bitOffset offset
    ext Signed = signExtend
    ext Unsigned = zeroExtend
