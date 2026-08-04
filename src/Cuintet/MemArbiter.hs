{- |
Arbitrates a single-port memory between instruction fetch and load/store
requests. See 'memArbiter'.
-}
module Cuintet.MemArbiter (Grant (..), MemArbiterReq (..), MemArbiterResp (..), memArbiter) where

import Clash.Prelude
import Cuintet.Eei
import Data.Maybe (isNothing)

-- | Which side was granted the memory on the previous cycle.
data Grant = ToInst | ToData
  deriving (Generic, NFDataX, Eq, Show)

data MemArbiterReq = MemArbiterReq
  { iReq :: Maybe InstReq
  -- ^ Instruction fetch request.
  , dReq :: Maybe DataReq
  -- ^ Load\/store request.
  , memResp :: MemBusResp MemDataWidth
  -- ^ Response from the memory.
  }
  deriving (Generic, NFDataX)

data MemArbiterResp = MemArbiterResp
  { memReq :: Maybe MemReq
  -- ^ Request to send to the memory.
  , iResp :: InstResp
  , dResp :: DataResp
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

>>> import Prelude
>>> import Clash.Prelude
>>> import Cuintet.Eei (MemBusReq (..), MemBusResp (..))
>>> mkI addr = Just MemBusReq{addr, wdata = Nothing}
>>> mkD addr wdata = Just MemBusReq{addr, wdata}
>>> reqs = [MemArbiterReq (mkI 0) Nothing (MemBusResp True Nothing), MemArbiterReq (mkI 4) (mkD 8 (Just 99)) (MemBusResp True (Just 0x11)), MemArbiterReq Nothing Nothing (MemBusResp True (Just 0x22)), MemArbiterReq (mkI 8) Nothing (MemBusResp True Nothing)]
>>> resps = simulateN @System (Prelude.length reqs) memArbiter reqs
>>> showReq r = (r.addr, r.wdata)
>>> (fmap showReq . (.memReq)) <$> resps
[Just (0,Nothing),Just (2,Just 0b0000_0000_0000_0000_0000_0000_0110_0011),Nothing,Just (2,Nothing)]
>>> (.rdata) . (.iResp) <$> resps
[Nothing,Just 0b0000_0000_0000_0000_0000_0000_0001_0001,Nothing,Nothing]
>>> (.rdata) . (.dResp) <$> resps
[Nothing,Nothing,Just 0b0000_0000_0000_0000_0000_0000_0010_0010,Nothing]
-}
memArbiter :: (HiddenClockResetEnable dom) => Signal dom MemArbiterReq -> Signal dom MemArbiterResp
memArbiter = mealy step Nothing
 where
  step :: Maybe Grant -> MemArbiterReq -> (Maybe Grant, MemArbiterResp)
  step grant MemArbiterReq{iReq, dReq, memResp = MemBusResp{ready, rdata}} =
    (grant', MemArbiterResp{memReq, iResp, dResp})
   where
    toMemReq MemBusReq{addr, wdata} = MemBusReq{addr = addrToMemAddr addr, wdata}
    memReq = (toMemReq <$> dReq) <|> (toMemReq <$> iReq)

    grant'
      | ready = (ToData <$ dReq) <|> (ToInst <$ iReq)
      | otherwise = grant

    iResp =
      MemBusResp
        { ready = ready && isNothing dReq
        , rdata = if grant == Just ToInst then rdata else Nothing
        }
    dResp =
      MemBusResp
        { ready
        , rdata = if grant == Just ToData then rdata else Nothing
        }
