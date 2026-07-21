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

data MemUnitReq = MemUnitReq
  { valid :: Bool
  -- ^ Whether a valid instruction is supplied.
  , isNew :: Bool
  -- ^ Whether the instruction is newly supplied this cycle.
  , ctrl :: InstCtrl
  , addr :: Addr
  -- ^ The access address computed by the ALU.
  , rs2 :: BitVector XLen
  -- ^ Data to store.
  , memresp :: MemBusResp XLen
  }
  deriving (Generic, NFDataX)

data MemUnitResp = MemUnitResp
  { rdata :: Maybe (BitVector XLen)
  , stall :: Bool
  -- ^ Whether the core must stall for an access in flight
  , memreq :: MemBusReq MemDataWidth XLen
  }
  deriving (Generic, NFDataX)

data State
  = -- | Wait for a new memory instruction; latch its request and move to 'WaitReady'.
    Init
  | -- | Keep sending the request until the memory accepts it, then move to 'WaitValid', with @(addr, wdata)@
    WaitReady Addr (Maybe (BitVector MemDataWidth))
  | -- | Wait until the access completes, then move back to 'Init'.
    WaitValid
  deriving (Generic, NFDataX, Eq)

memUnit :: (HiddenClockResetEnable dom) => Signal dom MemUnitReq -> Signal dom MemUnitResp
memUnit = mealy memUnitT Init
 where
  memUnitT :: State -> MemUnitReq -> (State, MemUnitResp)
  memUnitT state MemUnitReq{..} = (memUnitState, memUnitResp)
   where
    isNewMemOp = isNew && isMemOp ctrl
    memUnitState = case state of
      Init | isNewMemOp -> WaitReady addr (orNothing (isStoreOp ctrl) rs2)
      WaitReady _ _ | memresp.ready -> WaitValid
      WaitValid | isJust memresp.rdata -> Init
      _ -> state
    memUnitResp =
      MemUnitResp
        { rdata = memresp.rdata
        , -- in 'Init' when a new memory instruction arrives,
          -- in 'WaitReady' always,
          -- in 'WaitValid' until the response arrives.
          stall =
            valid && case state of
              Init -> isNewMemOp
              WaitReady _ _ -> True
              WaitValid -> isNothing memresp.rdata
        , memreq = case state of
            WaitReady reqAddr reqWdata -> MemBusReq{valid = True, addr = reqAddr, wdata = reqWdata}
            _ -> MemBusReq{valid = False, addr = undefined, wdata = undefined}
        }
