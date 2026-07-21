{- |
Memory unit: executes load/store instructions by sending the address
computed by the ALU to the memory bus.

An access takes at least 3 cycles ('Init' → 'WaitReady' → 'WaitValid'),
during which the core must stall (no write back, no instruction fetch).

The memory ignores the low 2 bits of the address, so misaligned LW/SW
do not work yet; all accesses are assumed to be 4-byte aligned.
-}
module Cuintet.MemUnit (MemUnitReq, MemUnitResp, memUnit) where

import Clash.Prelude
import Cuintet.Corectrl (InstCtrl, isMemOp, isStoreOp)
import Cuintet.Eei (Addr, MemBusReq (..), MemBusResp (..), MemDataWidth, XLen)
import Cuintet.Util (orNothing)
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

data State
  = -- | Wait for a new memory instruction; latch its request and move to 'WaitReady'.
    Init
  | -- | Keep sending the request until the memory accepts it, then move to 'WaitValid', with @(addr, wdata)@
    WaitReady Addr (Maybe (BitVector MemDataWidth))
  | -- | Wait until the access completes, then move back to 'Init'.
    WaitValid
  deriving (Generic, NFDataX)

memUnit :: (HiddenClockResetEnable dom) => Signal dom MemUnitReq -> Signal dom MemUnitResp
memUnit = mealy memUnitT Init
 where
  memUnitT :: State -> MemUnitReq -> (State, MemUnitResp)
  memUnitT state MemUnitReq{inst, memresp} = (memUnitState, memUnitResp)
   where
    isNewMemOp InstInfo{isNew, ctrl} = isNew && isMemOp ctrl
    memUnitState = case state of
      Init | Just i <- inst, isNewMemOp i -> WaitReady i.addr (orNothing (isStoreOp i.ctrl) i.rs2)
      WaitReady _ _ | memresp.ready -> WaitValid
      WaitValid | isJust memresp.rdata -> Init
      _ -> state
    memUnitResp =
      MemUnitResp
        { rdata = memresp.rdata
        , -- in 'Init' when a new memory instruction arrives,
          -- in 'WaitReady' always,
          -- in 'WaitValid' until the response arrives.
          stall = case (inst, state) of
            (Nothing, _) -> False
            (Just i, Init) -> isNewMemOp i
            (Just _, WaitReady _ _) -> True
            (Just _, WaitValid) -> isNothing memresp.rdata
        , memreq = case state of
            WaitReady reqAddr reqWdata -> Just MemBusReq{addr = reqAddr, wdata = reqWdata}
            _ -> Nothing
        }
