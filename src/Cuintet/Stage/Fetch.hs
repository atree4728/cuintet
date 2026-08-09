module Cuintet.Stage.Fetch (FetchState (..), initFetchState, FetchIn (..), FetchOut (..), fetch) where

import Clash.Prelude
import Cuintet.Eei (Addr, BusReq (..), BusResp (..), MemReq, MemResp, instAt)
import Cuintet.Fifo (FifoResp (..))
import Cuintet.Pipeline (IfId (..))
import Cuintet.Util (orNothing)
import Data.Maybe (isJust)

data FetchState = FetchState
  { next :: Addr
  , fetching :: Maybe Addr
  , staged :: Maybe IfId
  }
  deriving (Generic, NFDataX)

initFetchState :: FetchState
initFetchState =
  FetchState
    { next = 0
    , fetching = Nothing
    , staged = Nothing
    }

data FetchIn = FetchIn
  { iResp :: MemResp
  , fifo :: FifoResp IfId
  , redirect :: Maybe Addr
  }

data FetchOut = FetchOut
  { issue :: Maybe IfId
  , iReq :: Maybe MemReq
  }

fetch :: FetchState -> FetchIn -> (FetchState, FetchOut)
fetch FetchState {..} FetchIn {..} =
  ( FetchState {next = next', fetching = fetching', staged = staged'}
  , FetchOut {issue = staged, iReq}
  )
  where
    iReq = orNothing fifo.wreadyTwo BusReq {addr = next, wdata = Nothing}
    accepted = fifo.wreadyTwo && iResp.ready
    fetched = (,) <$> fetching <*> iResp.rdata

    (next', fetching')
      | Just target <- redirect = (target, Nothing)
      | accepted = (next + 4, Just next)
      | otherwise = (next, fetching)

    staged'
      | isJust redirect = Nothing
      | Just (addr, busWord) <- fetched = Just IfId {pc = addr, instBits = instAt addr busWord}
      | fifo.wready = Nothing -- the staged write was accepted
      | otherwise = staged -- pending
