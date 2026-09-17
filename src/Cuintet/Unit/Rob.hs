module Cuintet.Unit.Rob (RobStatic (..), RobDone (..), RobEntry (..), squashes, committedMapping, RobReq (..), RobResp (..), rob) where

import Clash.Prelude
import Control.Monad (guard)
import Cuintet.CoreCtrl (OpClass)
import Cuintet.Eei (Addr, CommitWidth, DispatchWidth, Inst, Mapping, MemReq, NRob, RobAddr, SystemOp (..), TrapCause, WriteBackWidth, XLen)
import Cuintet.Unit.MultiRam (multiRam)
import Cuintet.Util (orNothing)
import Data.Bool (bool)
import Data.Maybe (isJust, isNothing)

data RobStatic = RobStatic
  { pc :: Addr
  , mapping :: Maybe Mapping
  , systemOp :: Maybe SystemOp
  , opClass :: OpClass
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
  { allocates :: Vec DispatchWidth (Maybe (RobAddr, RobStatic))
  , completes :: Vec WriteBackWidth (Maybe (RobAddr, RobDone))
  , pop :: Index (CommitWidth + 1)
  , squash :: Bool
  }
  deriving (Generic, NFDataX)

data RobResp = RobResp
  { entries :: Vec CommitWidth (Maybe RobEntry)
  , hd :: RobAddr
  , tl :: RobAddr
  , free :: RobAddr
  }
  deriving (Generic, NFDataX)

data RobState = RobState
  { hd :: RobAddr
  , tl :: RobAddr
  }
  deriving (Generic, NFDataX)

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
rob req = mkResp <$> cur <*> multiRam addrs allocs <*> dones
  where
    (cur, allocs) = unbundle $ mealy step RobState {hd = 0, tl = 0} req

    step s@RobState {hd, tl} RobReq {..} = (RobState {hd = hd', tl = tl'}, (s, if squash then repeat Nothing else allocates))
      where
        hd' = hd + numConvert pop
        tl'
          | squash = hd'
          | otherwise = tl + sum (bool 0 1 . isJust <$> allocates)

    mkResp RobState {hd, tl} statics ds = RobResp {entries, hd, tl, free = maxBound - used}
      where
        used = tl - hd
        entries = izipWith (\i s d -> orNothing (numConvert i < used) (RobEntry s d)) statics ds

    addrs = (\RobState {hd} -> (hd +) . numConvert <$> indicesI @CommitWidth) <$> cur
    dones = zipWith orNothing <$> valids <*> multiRam addrs ((.completes) <$> req)
    valids = (\flags -> fmap (flags !!)) <$> completed <*> addrs

    completed = mealy flagStep (repeat @NRob False) (bundle (allocs, (.completes) <$> req))
    flagStep flags (as, cs) = (foldl assign flags (clears ++ sets), flags)
      where
        clears = fmap (\(robAddr, _) -> (robAddr, False)) <$> as
        sets = fmap (\(robAddr, _) -> (robAddr, True)) <$> cs
        assign f = maybe f (\(addr, v) -> replace addr v f)
{-# OPAQUE rob #-}
