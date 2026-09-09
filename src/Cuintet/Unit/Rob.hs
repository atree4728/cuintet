module Cuintet.Unit.Rob (RobStatic (..), RobDone (..), RobEntry (..)) where

import Clash.Prelude
import Cuintet.Eei (Addr, Inst, MemReq, SystemOp, TrapCause, XLen)
import Cuintet.Pipeline (Mapping)

data RobStatic = RobStatic
  { pc :: Addr
  , mapping :: Maybe Mapping
  , systemOp :: Maybe SystemOp
  , instBits :: Inst
  }

data RobDone = RobDone
  { exception :: Maybe (TrapCause, BitVector XLen)
  , mispredicted :: Bool
  , value :: BitVector XLen
  , mem :: Maybe MemReq
  }

data RobEntry = RobEntry
  { static :: RobStatic
  , done :: Maybe RobDone
  }
