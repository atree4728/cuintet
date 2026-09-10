module Cuintet.Unit.IssueQueue (IqTag (..), IqPayload (..), Select (..), IssueQueueReq (..), IssueQueueResp (..), issueQueue) where

import Clash.Prelude
import Control.Monad (guard)
import Cuintet.CoreCtrl (ExecUnit (..), InstCtrl, NExecUnits, OpClass, execUnit, fitsPort, nonSpeculative, opClassOf)
import Cuintet.Eei (Addr, DispatchWidth, IssueWidth, NRob, PRegAddr, RobAddr, TrapCause, XLen)
import Cuintet.Pipeline (Renamed (..))
import Cuintet.Unit.Btb (Prediction)
import Cuintet.Unit.MultiRam (multiRam)
import Cuintet.Upto (Upto)
import Cuintet.Upto qualified as Upto
import Cuintet.Util (orNothing)

data IqTag = IqTag
  { ready1, ready2 :: Bool
  , ps1Addr, ps2Addr :: PRegAddr
  , pdAddr :: Maybe PRegAddr
  , opClass :: OpClass
  }
  deriving (Generic, NFDataX)

data IqPayload = IqPayload
  { pc :: Addr
  , prediction :: Maybe Prediction
  , ctrl :: InstCtrl
  , imm :: BitVector XLen
  , exception :: Maybe (TrapCause, BitVector XLen)
  }
  deriving (Generic, NFDataX)

newtype Select = Select {selected :: Vec IssueWidth (Maybe (RobAddr, IqTag))}

oldest :: RobAddr -> Vec (n + 1) (Maybe (RobAddr, IqTag)) -> Maybe (RobAddr, IqTag)
oldest hd = fold pick
  where
    pick l r = case (l, r) of
      (Just (x, _), Just (y, _))
        | y - hd < x - hd -> r
        | otherwise -> l
      (Nothing, _) -> r
      _ -> l

without :: RobAddr -> Vec n (Maybe (RobAddr, IqTag)) -> Vec n (Maybe (RobAddr, IqTag))
without excluded = fmap (>>= \(addr, tag) -> orNothing (addr /= excluded) (addr, tag))

data IssueQueueReq = IssueQueueReq
  { dispatch :: Upto DispatchWidth Renamed
  , accepted :: Vec IssueWidth Bool
  , busy :: Vec NExecUnits Bool
  , robHead :: RobAddr
  , flush :: Bool
  }

newtype IssueQueueResp = IssueQueueResp {issue :: Vec IssueWidth (Maybe Renamed)}

step :: Vec NRob (Maybe IqTag) -> IssueQueueReq -> (Vec NRob (Maybe IqTag), Select)
step tags IssueQueueReq {..} = (tags', Select {selected})
  where
    entries = imap (\i tag -> (numConvert i :: RobAddr,) <$> tag) tags
    pick0 = do
      entry <- oldest robHead entries
      guard (candidate 0 entry)
      pure entry
    pick1 = do
      (robAddr0, tag0) <- pick0
      (robAddr1, tag1) <- oldest robHead (without robAddr0 entries)
      guard (candidate 1 (robAddr1, tag1))
      guard (Just tag1.ps1Addr /= tag0.pdAddr && Just tag1.ps2Addr /= tag0.pdAddr)
      pure (robAddr1, tag1)
    selected = pick0 :> pick1 :> Nil

    leaving = zipWith (\ok e -> guard ok *> (fst <$> e)) accepted selected

    oldestMem = fst <$> oldest robHead (memOnly <$> entries)
      where
        memOnly = (>>= \(robAddr, tag) -> orNothing (execUnit tag.opClass == Just MemUnit) (robAddr, tag))

    candidate port (robAddr, tag) =
      fitsPort port tag.opClass
        && maybe True (not . (busy !!) . fromEnum) (execUnit tag.opClass)
        && (not (nonSpeculative tag.opClass) || robAddr == robHead)
        && (execUnit tag.opClass /= Just MemUnit || Just robAddr == oldestMem)

    tags'
      | flush = repeat Nothing
      | otherwise = inserted
      where
        cleared = foldl clear tags leaving
        clear ts = maybe ts (\a -> replace a Nothing ts)

        inserted = foldl insert cleared (Upto.toMaybes dispatch)
        insert ts = maybe ts (\r -> replace r.robAddr (Just (tagOf r)) ts)

tagOf :: Renamed -> IqTag
tagOf Renamed {..} = IqTag {ready1 = True, ready2 = True, ps1Addr, ps2Addr, pdAddr, opClass = opClassOf ctrl}

issueQueue :: (HiddenClockResetEnable dom) => Signal dom IssueQueueReq -> Signal dom IssueQueueResp
issueQueue req = mkOut <$> selection <*> multiRam (addrs <$> selection) (writes <$> req)
  where
    mkOut Select {selected} payloads = IssueQueueResp {issue = zipWith toRenamed payloads selected}
    toRenamed IqPayload {..} = fmap $ \(robAddr, IqTag {..}) -> Renamed {..}

    selection = mealy step (repeat Nothing) req

    addrs Select {selected} = maybe 0 fst <$> selected

    writes IssueQueueReq {dispatch} = fmap payloadOf <$> Upto.toMaybes dispatch
    payloadOf Renamed {..} = (robAddr, IqPayload {..})
