{-# LANGUAGE StrictData #-}

-- | A Konata pipeline log, reconstructed from the per-clock 'CoreTrace'.
module Cuintet.Debug.Konata (konataLog) where

import Clash.Prelude (natToNum)
import Cuintet.Core (CoreTrace (..))
import Cuintet.Debug.Show (hex, retireLines)
import Cuintet.Eei (Addr, Inst, IssueWidth)
import Cuintet.Pipeline (IfId (..), Retire (..))
import Cuintet.Upto qualified as Upto
import Data.Foldable (toList)
import Data.Function (applyWhen)
import Data.Maybe (catMaybes, fromMaybe, isJust, maybeToList)
import Text.Printf (printf)
import Prelude

data Inflight = Inflight
  { instId :: Int
  , pc :: Addr
  , instBits :: Maybe Inst
  }

data Stage = IF | Fs | Iq | ID | Ex | Ma | Cm
  deriving (Eq, Show)

-- | Every stage but IF and Fs holds a group, since a fetch is one bus word and everything after it is one issue group.
data Model = Model
  { nextId :: Int
  , commits :: Int
  , fetching :: Maybe Inflight
  , staged :: Maybe Inflight
  , ifIdQ :: [Inflight]
  , idExQ :: [Inflight]
  , exMaQ :: [Inflight]
  , maCmQ :: [Inflight]
  }

initModel :: Model
initModel =
  Model
    { nextId = 0
    , commits = 0
    , fetching = Nothing
    , staged = Nothing
    , ifIdQ = []
    , idExQ = []
    , exMaQ = []
    , maCmQ = []
    }

issued :: Maybe Inflight -> Inflight
issued = fromMaybe (errorWithoutStackTrace "konataLog: lost track of the pipeline")

shift :: Bool -> Bool -> Maybe Inflight -> Maybe Inflight -> Maybe Inflight
shift push pop src cur
  | push = Just (issued src)
  | pop = Nothing
  | otherwise = cur

-- | A group leaves a stage whole, so a stage holds either what the one upstream just handed it or what it already had.
move :: Int -> [Inflight] -> Int -> [Inflight] -> [Inflight]
move push src pop cur
  | push > 0 = take push src
  | pop > 0 = []
  | otherwise = cur

modelStep :: Model -> CoreTrace -> Model
modelStep Model {..} CoreTrace {..} = applyWhen flush flushed moved
  where
    flushed x = x {fetching = Nothing, staged = Nothing, ifIdQ = [], idExQ = []}

    retires = catMaybes (toList retired)
    allocated = if isJust fetchStart then 1 else 0

    -- one fetch carries up to two instructions but only one id, so the rest are numbered as they enter the buffer
    entered = zipWith inflight ids (catMaybes (toList (Upto.toMaybes ifIssue)))
      where
        ids = map (.instId) (maybeToList staged) <> [nextId + allocated ..]
        inflight i e = Inflight {instId = i, pc = e.pc, instBits = Just e.instBits}

    moved =
      Model
        { nextId = nextId + allocated + length (drop 1 entered)
        , commits = commits + length retires
        , fetching = case fetchStart of
            Just pc -> Just Inflight {instId = nextId, pc, instBits = Nothing}
            Nothing -> if fetchDone then Nothing else fetching
        , staged = shift fetchDone (not (null entered)) fetching staged
        , ifIdQ = drop (count idIssue) ifIdQ <> entered
        , idExQ = move (count idIssue) ifIdQ (count exIssue) idExQ
        , exMaQ = move (count exIssue) idExQ (count maIssue) exMaQ
        , maCmQ = move (count maIssue) exMaQ (length retires) maCmQ
        }

count :: (Integral a) => a -> Int
count = fromIntegral

stages :: Model -> [(Inflight, Stage)]
stages Model {..} =
  concat
    [ slot IF (maybeToList fetching)
    , slot Fs (maybeToList staged)
    , zip ifIdQ (replicate (natToNum @IssueWidth) ID <> repeat Iq)
    , slot Ex idExQ
    , slot Ma exMaQ
    , slot Cm maCmQ
    ]
  where
    slot s is = [(i, s) | i <- is]

label :: Addr -> Maybe Inst -> String
label pc bits = printf "%s: %s" (hex pc) (maybe "(not fetched)" hex bits)

clockLines :: CoreTrace -> [(Inflight, Stage)] -> Model -> [String]
clockLines CoreTrace {..} was cur@Model {..} = concatMap entering (stages cur) <> retiredLog <> concatMap lostLines lost
  where
    seen = [(i.instId, s) | (i, s) <- was]
    entering (i, s) = case lookup i.instId seen of
      Just u | u == s -> []
      Just _ -> [sLine i s]
      Nothing -> [printf "I\t%d\t%d\t0" i.instId i.instId, sLine i s]

    sLine i s = printf "S\t%d\t0\t%s" i.instId (show s)

    retiredLog = concat (zipWith3 ofRetire [0 ..] maCmQ (catMaybes (toList retired)))
      where
        ofRetire k i l =
          printf "L\t%d\t0\t%s" i.instId (label l.pc (Just l.instBits))
            : [printf "L\t%d\t1\t%s" i.instId ln | ln <- retireLines l]
              <> [printf "R\t%d\t%d\t0" i.instId (commits + k)]

    -- EX drops the lanes younger than a redirect or a trap; a flush drops everything still upstream of MA
    lost = squashed <> flushedOut
      where
        squashed = if count exIssue > 0 then drop (count exIssue) idExQ else []
        flushedOut
          | flush = (if count exIssue > 0 then [] else idExQ) <> ifIdQ <> maybeToList staged <> maybeToList fetching
          | otherwise = []

    lostLines i = [printf "L\t%d\t0\t%s" i.instId (label i.pc i.instBits), printf "R\t%d\t%d\t1" i.instId i.instId]

konataLog :: [CoreTrace] -> [String]
konataLog ts = "Kanata\t0004" : "C=\t0" : concat (zipWith3 clock ts ([] : map stages models) models)
  where
    models = scanl modelStep initModel ts
    clock t was cur = clockLines t was cur <> ["C\t1"]
