-- | Load\/store unit: executes load\/store instructions by sending the address computed by the ALU to the memory bus.
module Cuintet.Unit.LoadStore (
  LoadStoreJob (..),
  LoadShape (..),
  Width (..),
  Sign (..),
  LoadStoreReq (..),
  LoadStoreResp (..),
  LoadStoreState (..),
  loadStoreStep,
  loadResult,
) where

import Clash.Prelude
import Cuintet.CoreCtrl (InstCtrl (..), isMemOp)
import Cuintet.Eei (Addr, BusReq (..), BusResp (..), LaneOffset, LoadShape (..), MemDataBytes, MemOp (..), MemReq, MemResp, Sign (..), StoreLanes (..), Width (..), XLen, aligned, bitOffset, laneMask, laneOffset)
import Cuintet.Util (orNothing)
import Data.Maybe (isJust, isNothing)

-- | One memory access for the unit to carry out.
data LoadStoreJob = LoadStoreJob
  { ctrl :: InstCtrl
  , addr :: Addr
  -- ^ The access address computed by the ALU.
  , wdata :: BitVector XLen
  -- ^ Data to store.
  }
  deriving (Generic, NFDataX)

data LoadStoreReq = LoadStoreReq
  { job :: Maybe LoadStoreJob
  -- ^ The access to carry out, if any.
  , memResp :: MemResp
  }
  deriving (Generic, NFDataX)

data LoadStoreResp = LoadStoreResp
  { result :: Maybe (BitVector XLen)
  -- ^ The value a completed load produced.
  , stall :: Bool
  -- ^ Whether the core must stall for an access in flight
  , memReq :: Maybe MemReq
  }
  deriving (Generic, NFDataX)

-- | An access as the bus needs it: the lanes to write, or the shape to give the word that comes back.
data BusAccess
  = BusStore (StoreLanes MemDataBytes)
  | BusLoad LoadShape
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
loadStoreStep state LoadStoreReq {job, memResp} = (memUnitState, memUnitResp)
  where
    memUnitState = case state of
      Idle | Just i <- job, Just acc <- i.ctrl.memOp -> WaitReady i.addr (busAccess acc i.addr i.wdata)
      WaitReady _ acc | memResp.ready -> WaitValid acc
      WaitValid _ | isJust memResp.rdata -> Idle
      _ -> state
    memUnitResp =
      LoadStoreResp
        { result = case state of
            WaitValid (BusLoad fmt) -> loadResult fmt <$> memResp.rdata
            _ -> Nothing
        , -- in 'Idle' when a new memory instruction arrives,
          -- in 'WaitReady' always,
          -- in 'WaitValid' until the response arrives.
          stall = case (job, state) of
            (Nothing, _) -> False
            (Just i, Idle) -> isMemOp i.ctrl
            (Just _, WaitReady _ _) -> True
            (Just _, WaitValid _) -> isNothing memResp.rdata
        , memReq = case state of
            WaitReady reqAddr (BusLoad _) -> Just BusReq {addr = reqAddr, wdata = Nothing}
            WaitReady reqAddr (BusStore wdata) -> Just BusReq {addr = reqAddr, wdata = Just wdata}
            _ -> Nothing
        }

-- | The decoded access put in the form the bus needs; the offset is the one part of it the address supplies.
busAccess :: MemOp -> Addr -> BitVector (MemDataBytes * 8) -> BusAccess
busAccess acc addr wdata = case acc of
  Store width -> checked width $ BusStore (storeLanes width offset wdata)
  Load width sign -> checked width $ BusLoad LoadShape {width, sign, offset}
  where
    offset = laneOffset addr
    checked width x
      | aligned width offset = x
      | otherwise = deepErrorX "busAccess: misaligned access"

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
