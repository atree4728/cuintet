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
  storeLanes,
) where

import Clash.Prelude
import Cuintet.Eei (Addr, BusReq (..), BusResp (..), LaneOffset, LoadShape (..), MemDataBytes, MemOp (..), MemReq, MemResp, PRegAddr, RobAddr, Sign (..), StoreLanes (..), Width (..), XLen, aligned, bitOffset, laneMask, laneOffset)
import Cuintet.Forwarding (Forwarding)
import Cuintet.Forwarding qualified as F
import Cuintet.Pipeline (Completion (..))
import Cuintet.Unit.Rob (RobDone (..))
import Cuintet.Util (orNothing)

data LoadStoreJob = LoadStoreJob
  { memOp :: MemOp
  , addr :: Addr
  , wdata :: BitVector XLen
  , pdAddr :: Maybe PRegAddr
  , robAddr :: RobAddr
  , mispredicted :: Bool
  }
  deriving (Generic, NFDataX)

data LoadStoreState
  = Idle
  | WaitReady LoadStoreJob BusAccess
  | WaitValid LoadStoreJob BusAccess
  | Waiting Completion
  deriving (Generic, NFDataX)

data LoadStoreReq = LoadStoreReq
  { job :: Maybe LoadStoreJob
  , memResp :: MemResp
  , granted :: Bool
  , squash :: Bool
  }

data LoadStoreResp = LoadStoreResp
  { busy :: Bool
  , done :: Maybe Completion
  , forwarding :: Forwarding
  , memReq :: Maybe MemReq
  }

-- | An access as the bus needs it: the lanes to write, or the shape to give the word that comes back.
data BusAccess
  = BusStore (StoreLanes MemDataBytes)
  | BusLoad LoadShape
  deriving (Generic, NFDataX)

busReq :: Addr -> BusAccess -> MemReq
busReq addr (BusStore wdata) = BusReq {addr, wdata = Just wdata}
busReq addr (BusLoad _) = BusReq {addr, wdata = Nothing}

loadStoreStep :: LoadStoreState -> LoadStoreReq -> (LoadStoreState, LoadStoreResp)
loadStoreStep state LoadStoreReq {job, memResp, granted, squash}
  | squash = (Idle, nop)
  | otherwise = case state of
      Idle -> (maybe Idle (\j -> WaitReady j (busAccess j.memOp j.addr j.wdata)) job, nop)
      WaitReady j acc ->
        (if memResp.ready then WaitValid j acc else state, inflight j.pdAddr (Just (busReq j.addr acc)))
      WaitValid j acc -> case memResp.rdata of
        Nothing -> (state, inflight j.pdAddr Nothing)
        Just w -> settle (completion j acc w)
      Waiting c -> settle c
  where
    nop = LoadStoreResp {busy = False, done = Nothing, forwarding = F.Idle, memReq = Nothing}
    inflight pdAddr memReq = LoadStoreResp {busy = True, done = Nothing, forwarding = F.forwarding pdAddr Nothing, memReq}
    settle c =
      ( if granted then Idle else Waiting c
      , LoadStoreResp {busy = True, done = Just c, forwarding = F.broadcast c, memReq = Nothing}
      )

completion :: LoadStoreJob -> BusAccess -> BitVector (MemDataBytes * 8) -> Completion
completion LoadStoreJob {..} acc busWord =
  Complete robAddr pdAddr RobDone {exception = Nothing, mispredicted, value, mem = Just (busReq addr acc)}
  where
    value = case acc of
      BusLoad shape -> loadResult shape busWord
      BusStore _ -> deepErrorX "loadStore: a store produces no result"

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
