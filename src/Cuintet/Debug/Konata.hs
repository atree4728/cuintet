{-# LANGUAGE StrictData #-}

-- | A Konata pipeline log, reconstructed from the per-clock 'CoreTrace'.
module Cuintet.Debug.Konata (konataLog) where

import Cuintet.Core (CoreTrace (..))
import Cuintet.Debug.Show (hex, retireLines)
import Cuintet.Eei (Addr, Inst)
import Cuintet.Pipeline (IfId (..), Retire (..))
import Data.Function (applyWhen)
import Data.Maybe (fromMaybe, isJust, listToMaybe, maybeToList)
import Text.Printf (printf)
import Prelude

data Inflight = Inflight
  { instId :: Int
  , pc :: Addr
  , instBits :: Maybe Inst
  }

data Stage = IF | Fs | Iq | ID | Ex | Ma | Cm
  deriving (Eq, Show)

data Model = Model
  { nextId :: Int
  , commits :: Int
  , fetching :: Maybe Inflight
  , staged :: Maybe Inflight
  , ifIdQ :: [Inflight]
  , idExQ :: Maybe Inflight
  , exMaQ :: Maybe Inflight
  , maWbQ :: Maybe Inflight
  }

initModel :: Model
initModel =
  Model
    { nextId = 0
    , commits = 0
    , fetching = Nothing
    , staged = Nothing
    , ifIdQ = []
    , idExQ = Nothing
    , exMaQ = Nothing
    , maWbQ = Nothing
    }

issued :: Maybe Inflight -> Inflight
issued = fromMaybe (errorWithoutStackTrace "konataLog: lost track of the pipeline")

shift :: Bool -> Bool -> Maybe Inflight -> Maybe Inflight -> Maybe Inflight
shift push pop src cur
  | push = Just (issued src)
  | pop = Nothing
  | otherwise = cur

modelStep :: Model -> CoreTrace -> Model
modelStep Model {..} CoreTrace {..} = applyWhen flush flushed moved
  where
    flushed x = x {fetching = Nothing, staged = Nothing, ifIdQ = [], idExQ = Nothing}

    moved =
      Model
        { nextId = applyWhen (isJust fetchStart) (+ 1) nextId
        , commits = applyWhen (any isJust retired) (+ 1) commits
        , fetching = case fetchStart of
            Just pc -> Just Inflight {instId = nextId, pc, instBits = Nothing}
            Nothing -> if fetchDone then Nothing else fetching
        , staged = shift fetchDone (isJust ifIssue) fetching staged
        , ifIdQ = ifIdQ'
        , idExQ = shift idIssue exIssue (listToMaybe ifIdQ) idExQ
        , exMaQ = shift exIssue maIssue idExQ exMaQ
        , maWbQ = shift maIssue (any isJust retired) exMaQ maWbQ
        }

    ifIdQ' = case ifIssue of
      Just entry -> popped <> [fetched (issued staged) entry.instBits]
      Nothing -> popped
      where
        popped = if idIssue then drop 1 ifIdQ else ifIdQ
        fetched i bits = Inflight {instId = i.instId, pc = i.pc, instBits = Just bits}

stages :: Model -> [(Inflight, Stage)]
stages Model {..} =
  concat
    [ slot IF fetching
    , slot Fs staged
    , zip ifIdQ (ID : repeat Iq)
    , slot Ex idExQ
    , slot Ma exMaQ
    , slot Cm maWbQ
    ]
  where
    slot s mi = [(i, s) | i <- maybeToList mi]

label :: Addr -> Maybe Inst -> String
label pc bits = printf "%s: %s" (hex pc) (maybe "(not fetched)" hex bits)

clockLines :: CoreTrace -> [(Inflight, Stage)] -> Model -> [String]
clockLines CoreTrace {..} was cur@Model {..} = concatMap entering (stages cur) <> retiredLog <> flushed
  where
    seen = [(i.instId, s) | (i, s) <- was]
    entering (i, s) = case lookup i.instId seen of
      Just u | u == s -> []
      Just _ -> [sLine i s]
      Nothing -> [printf "I\t%d\t%d\t0" i.instId i.instId, sLine i s]

    sLine i s = printf "S\t%d\t0\t%s" i.instId (show s)

    retiredLog = concatMap ofRetire retired
      where
        ofRetire = \case
          Nothing -> []
          Just l ->
            let i = issued maWbQ
             in printf "L\t%d\t0\t%s" i.instId (label l.pc (Just l.instBits))
                  : [printf "L\t%d\t1\t%s" i.instId ln | ln <- retireLines l]
                    <> [printf "R\t%d\t%d\t0" i.instId commits]

    flushed
      | flush = concatMap flushLines (maybeToList idExQ <> ifIdQ <> maybeToList staged <> maybeToList fetching)
      | otherwise = []

    flushLines i = [printf "L\t%d\t0\t%s" i.instId (label i.pc i.instBits), printf "R\t%d\t%d\t1" i.instId i.instId]

konataLog :: [CoreTrace] -> [String]
konataLog ts = "Kanata\t0004" : "C=\t0" : concat (zipWith3 clock ts ([] : map stages models) models)
  where
    models = scanl modelStep initModel ts
    clock t was cur = clockLines t was cur <> ["C\t1"]
