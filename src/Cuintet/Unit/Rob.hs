module Cuintet.Unit.Rob (RobStatic (..), RobDone (..), RobEntry (..), squashes, committedMapping, RobReq (..), RobResp (..), rob) where

import Clash.Prelude
import Control.Monad (guard)
import Cuintet.Eei (Addr, CommitWidth, DispatchWidth, Inst, Mapping, MemReq, NRob, RobAddr, SystemOp (..), TrapCause, WriteBackWidth, XLen)
import Cuintet.Unit.MultiRam (multiRam)
import Cuintet.Unit.Ring (RingReq (..), RingResp (..), ring)
import Cuintet.Upto (Upto (..))
import Cuintet.Upto qualified as Upto
import Cuintet.Util (orNothing)
import Data.Maybe (isJust, isNothing)

data RobStatic = RobStatic
  { pc :: Addr
  , mapping :: Maybe Mapping
  , systemOp :: Maybe SystemOp
  , instBits :: Inst
  }
  deriving (Generic, NFDataX)

data RobDone = RobDone
  { exception :: Maybe (TrapCause, BitVector XLen)
  , mispredicted :: Bool
  , value :: BitVector XLen
  , mem :: Maybe MemReq
  }
  deriving (Generic, NFDataX)

data RobEntry = RobEntry
  { static :: RobStatic
  , done :: Maybe RobDone
  }
  deriving (Generic, NFDataX)

data RobReq = RobReq
  { allocates :: Upto DispatchWidth RobStatic
  , completes :: Vec WriteBackWidth (Maybe (RobAddr, RobDone))
  , pop :: Index (CommitWidth + 1)
  , squash :: Bool
  }
  deriving (Generic, NFDataX)

newtype RobResp = RobResp {buffer :: RingResp (BitSize RobAddr) CommitWidth RobEntry}
  deriving newtype (Generic, NFDataX)

squashes :: RobEntry -> Bool
squashes RobEntry {..} = any squashing done
  where
    squashing RobDone {..} = isJust exception || static.systemOp == Just SysMret || mispredicted

committedMapping :: RobEntry -> Maybe Mapping
committedMapping RobEntry {..} = do
  RobDone {exception} <- done
  guard (isNothing exception)
  static.mapping

rob :: forall dom. (HiddenClockResetEnable dom) => Signal dom RobReq -> Signal dom RobResp
rob req = mkResp <$> statics <*> dones
  where
    mkResp resp@RingResp {rdata} ds = RobResp resp {rdata = rdata {elems = zipWith RobEntry rdata.elems ds}}

    statics = ring (SNat @(BitSize RobAddr)) (mkRingReq <$> req)
    mkRingReq RobReq {..} = RingReq {wdata = allocates, pop, squash}

    addrs = (\RingResp {hd} -> (hd +) . numConvert <$> indicesI @CommitWidth) <$> statics
    dones = zipWith orNothing <$> valids <*> multiRam addrs ((.completes) <$> req)
    valids = (\flags -> fmap (flags !!)) <$> completed <*> addrs

    completed = mealy step (repeat @NRob False) (bundle (statics, req))
    step flags (RingResp {tl}, RobReq {allocates, completes}) = (foldl assign flags (clears ++ sets), flags)
      where
        clears = imap (\i -> fmap (const (tl + numConvert i, False))) (Upto.toMaybes allocates)
        sets = fmap (\(robAddr, _) -> (robAddr, True)) <$> completes
        assign f = maybe f (\(addr, v) -> replace addr v f)
{-# OPAQUE rob #-}
