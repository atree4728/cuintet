module Cuintet.Unit.IssueQueue (NBroadcast, IqTag (..), IqPayload (..), Select (..), IssueQueueReq (..), IssueQueueResp (..), issueQueue) where

import Clash.Prelude
import Cuintet.CoreCtrl (InstCtrl, NExecUnits, OpClass, execUnit, fitsPort, opClassOf, usesRs1, usesRs2)
import Cuintet.Eei (Addr, DispatchWidth, IssueWidth, LoadQueueAddr, NPRegs, NRob, PRegAddr, RobAddr, StoreQueueAddr, TrapCause, XLen)
import Cuintet.Pipeline (Renamed (..))
import Cuintet.Unit.Btb (Prediction)
import Cuintet.Unit.MultiRam (multiRam)
import Cuintet.Util (orNothing, (<<$>>))

-- | AtIssue from both ports, AtComplete from both units, AtCommit.
type NBroadcast = 5

data IqTag = IqTag
  { ready1, ready2 :: Bool
  , ps1Addr, ps2Addr :: PRegAddr
  , opClass :: OpClass
  }
  deriving (Generic, NFDataX)

data IqPayload = IqPayload
  { pc :: Addr
  , prediction :: Maybe Prediction
  , ctrl :: InstCtrl
  , imm :: BitVector XLen
  , exception :: Maybe (TrapCause, BitVector XLen)
  , pdAddr :: Maybe PRegAddr
  , sqAddr :: StoreQueueAddr
  , lqAddr :: LoadQueueAddr
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
  { dispatch :: Vec DispatchWidth (Maybe Renamed)
  , accepted :: Vec IssueWidth Bool
  , busy :: Vec NExecUnits Bool
  , robHead :: RobAddr
  , wakeup :: Vec NBroadcast (Maybe PRegAddr)
  , squash :: Bool
  }

newtype IssueQueueResp = IssueQueueResp {issue :: Vec IssueWidth (Maybe Renamed)}

data IssueQueueState = IssueQueueState
  { tags :: Vec NRob (Maybe IqTag)
  , ready :: Vec NPRegs Bool
  }
  deriving (Generic, NFDataX)

initState :: IssueQueueState
initState = IssueQueueState {tags = repeat Nothing, ready = repeat True}

step :: IssueQueueState -> IssueQueueReq -> (IssueQueueState, Select)
step IssueQueueState {..} IssueQueueReq {..} = (IssueQueueState {tags = tags', ready = ready'}, Select {selected})
  where
    entries = imap (\i tag -> (numConvert i :: RobAddr,) <$> tag) tags

    candidates port = (>>= \e -> orNothing (candidate port e) e) <$> entries
    pick0 = oldest robHead (candidates 0)
    pick1 = oldest robHead (maybe id (without . fst) pick0 (candidates 1))
    selected = pick0 :> pick1 :> Nil

    leaving = zipWith (\ok e -> if ok then fst <$> e else Nothing) accepted selected

    candidate port (_, tag) =
      tag.ready1
        && tag.ready2
        && fitsPort port tag.opClass
        && maybe True (not . (busy !!) . fromEnum) (execUnit tag.opClass)

    -- Set before clear: a tag allocated in this clock is not ready.
    ready' = foldl (mark False) (foldl (mark True) ready wakeup) ((>>= (.pdAddr)) <$> dispatch)
      where
        mark v rs = maybe rs (\p -> replace p v rs)

    hit ps = Just ps `elem` wakeup
    woken tag = tag {ready1 = tag.ready1 || hit tag.ps1Addr, ready2 = tag.ready2 || hit tag.ps2Addr}

    tagOf Renamed {..} =
      IqTag
        { ready1 = not (usesRs1 ctrl) || ready' !! ps1Addr
        , ready2 = not (usesRs2 ctrl) || ready' !! ps2Addr
        , ps1Addr
        , ps2Addr
        , opClass = opClassOf ctrl
        }

    tags'
      | squash = repeat Nothing
      | otherwise = foldl insert (woken <<$>> cleared) dispatch
      where
        cleared = foldl (\ts -> maybe ts (\a -> replace a Nothing ts)) tags leaving
        insert ts = maybe ts (\r -> replace r.robAddr (Just (tagOf r)) ts)

issueQueue :: (HiddenClockResetEnable dom) => Signal dom IssueQueueReq -> Signal dom IssueQueueResp
issueQueue req = mkOut <$> selection <*> multiRam (addrs <$> selection) (writes <$> req)
  where
    mkOut Select {selected} payloads = IssueQueueResp {issue = zipWith toRenamed payloads selected}
    toRenamed IqPayload {..} = fmap $ \(robAddr, IqTag {..}) -> Renamed {..}

    selection = mealy step initState req

    addrs Select {selected} = maybe 0 fst <$> selected

    writes IssueQueueReq {dispatch} = payloadOf <<$>> dispatch
    payloadOf Renamed {..} = (robAddr, IqPayload {..})
