{- |
Arbitrates a single-port memory between instruction fetch and load/store
requests. See 'busArbiter'.
-}
module Cuintet.BusArbiter (Grant (..), BusArbiterReq (..), BusArbiterResp (..), busArbiter) where

import Clash.Prelude
import Cuintet.Eei (BusResp (..), MemReq, MemResp)
import Data.Maybe (isNothing)

-- | Which side was granted the memory on the previous cycle.
data Grant = ToInst | ToData
  deriving (Generic, NFDataX, Eq, Show)

data BusArbiterReq = BusArbiterReq
  { iReq :: Maybe MemReq
  -- ^ Instruction fetch request.
  , dReq :: Maybe MemReq
  -- ^ Load\/store request.
  , memResp :: MemResp
  -- ^ Response from the memory.
  }
  deriving (Generic, NFDataX)

data BusArbiterResp = BusArbiterResp
  { iResp :: MemResp
  , dResp :: MemResp
  , memReq :: Maybe MemReq
  -- ^ Request to send to the memory.
  }
  deriving (Generic, NFDataX)

{- | Arbitrates instruction fetch ('iReq') and load\/store ('dReq') requests
onto a single-port memory, giving 'dReq' priority so that instruction fetch
does not starve memory instructions.

The memory returns 'rdata' one cycle after accepting a request, but the
response itself carries no destination tag. So the arbiter remembers, in a
'Grant' register, which side was granted last, and routes the next 'rdata' to
that side. 'iResp'\'s @ready@ is masked while 'dReq' is present, so a
fetch request is only accepted once no load\/store is contending; 'dResp'
always sees the memory's @ready@ directly, since 'dReq' is never masked.
-}
busArbiter :: (HiddenClockResetEnable dom) => Signal dom BusArbiterReq -> Signal dom BusArbiterResp
busArbiter = mealy step Nothing
  where
    step :: Maybe Grant -> BusArbiterReq -> (Maybe Grant, BusArbiterResp)
    step grant BusArbiterReq {iReq, dReq, memResp = BusResp {ready, rdata}} =
      (grant', BusArbiterResp {memReq, iResp, dResp})
      where
        memReq = dReq <|> iReq

        grant'
          | ready = (ToData <$ dReq) <|> (ToInst <$ iReq)
          | otherwise = grant

        iResp =
          BusResp
            { ready = ready && isNothing dReq
            , rdata = if grant == Just ToInst then rdata else Nothing
            }
        dResp =
          BusResp
            { ready
            , rdata = if grant == Just ToData then rdata else Nothing
            }
