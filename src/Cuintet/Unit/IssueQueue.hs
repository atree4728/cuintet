module Cuintet.Unit.IssueQueue (IqTag (..), IqPayload (..), Select (..), IssueQueueReq (..), IssueQueueResp (..), issueQueue) where

import Clash.Prelude
import Control.Monad (guard)
import Cuintet.CoreCtrl (InstCtrl, OpClass, fitsPort, opClassOf)
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
  , robHead :: RobAddr
  , flush :: Bool
  }

newtype IssueQueueResp = IssueQueueResp {issue :: Vec IssueWidth (Maybe Renamed)}

step :: Vec NRob (Maybe IqTag) -> IssueQueueReq -> (Vec NRob (Maybe IqTag), Select)
step tags IssueQueueReq {..} = (tags', Select {selected})
  where
    entries = imap (\i tag -> (numConvert i :: RobAddr,) <$> tag) tags
    pick0 = oldest robHead entries
    pick1 = do
      (robAddr0, tag0) <- pick0
      e@(_, tag) <- oldest robHead (without robAddr0 entries)
      guard (fitsPort 1 tag.opClass)
      -- Both entries are read the same cycle they're selected, so pick1 can't yet see a
      -- result pick0 produces this cycle: don't pair them, or pick1 would read stale data.
      guard (Just tag.ps1Addr /= tag0.pdAddr && Just tag.ps2Addr /= tag0.pdAddr)
      pure e
    selected = pick0 :> pick1 :> Nil

    leaving = zipWith (\ok e -> guard ok *> (fst <$> e)) accepted selected

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
