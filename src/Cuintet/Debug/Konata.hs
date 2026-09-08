{-# LANGUAGE StrictData #-}

-- | A Konata pipeline log, reconstructed from the per-clock 'CoreTrace'.
module Cuintet.Debug.Konata (konataLog) where

import Clash.Prelude (natToNum)
import Cuintet.Core (CoreTrace (..))
import Cuintet.Debug.Show (hex, retireLines)
import Cuintet.Eei (Addr, FetchWidth, Inst)
import Cuintet.Pipeline (Fetched (..), Retire (..))
import Cuintet.Unit.Btb (bankOf)
import Cuintet.Upto qualified as Upto
import Data.Foldable (toList)
import Data.Function (applyWhen)
import Data.Maybe (catMaybes)
import Text.Printf (printf)
import Prelude

data Inflight = Inflight
  { instId :: Int
  , pc :: Addr
  , instBits :: Maybe Inst
  }

data Stage = IF | ID | RN | RR | EX | MA | Cm
  deriving (Eq, Show)

-- | Where everything in flight is. IF holds one list per fetch, since a fetch enters the fetch buffer whole.
data Model = Model
  { nextId :: Int
  , commits :: Int
  , ifQ :: [[Inflight]]
  , idQ :: [Inflight]
  , rnQ :: [Inflight]
  , rrQ :: [Inflight]
  , exQ :: [Inflight]
  , maQ :: [Inflight]
  , cmQ :: [Inflight]
  }

initModel :: Model
initModel = Model {nextId = 0, commits = 0, ifQ = [], idQ = [], rnQ = [], rrQ = [], exQ = [], maQ = [], cmQ = []}

-- | The instructions a fetch brings back: the slots of the bus word from its address up, as 'Cuintet.Eei.instSlice' cuts them.
fetchGroup :: Int -> Addr -> [Inflight]
fetchGroup firstId addr =
  [ Inflight {instId = firstId + i, pc = addr + 4 * fromIntegral i, instBits = Nothing}
  | i <- [0 .. natToNum @FetchWidth - 1 - fromIntegral (bankOf addr)]
  ]

-- | What the oldest fetch hands the fetch buffer this clock, and the rest of it, cut off by a predicted-taken branch.
handedOver :: Model -> CoreTrace -> ([Inflight], [Inflight])
handedOver Model {ifQ} CoreTrace {ifIssue}
  | null entries = ([], [])
  | otherwise = (zipWith fill group entries, drop (length entries) group)
  where
    group = concat (take 1 ifQ)
    entries = catMaybes (toList (Upto.toMaybes ifIssue))
    fill i e
      | i.pc == e.pc = Inflight {instId = i.instId, pc = e.pc, instBits = Just e.instBits}
      | otherwise = errorWithoutStackTrace "konataLog: lost track of the fetch path"

-- | A group leaves a stage whole, so a stage holds either what the one upstream just handed it or what it already had.
move :: Int -> [Inflight] -> Int -> [Inflight] -> [Inflight]
move push src pop cur
  | push > 0 = take push src
  | pop > 0 = []
  | otherwise = cur

modelStep :: Model -> CoreTrace -> Model
modelStep model@Model {..} trace@CoreTrace {..} = applyWhen flush flushed moved
  where
    flushed x = x {ifQ = [], idQ = [], rnQ = [], rrQ = [], exQ = []}

    retires = length (catMaybes (toList retired))
    entered = fst (handedOver model trace)
    started = maybe [] (\addr -> [fetchGroup nextId addr]) fetchStart

    moved =
      Model
        { nextId = nextId + length (concat started)
        , commits = commits + retires
        , ifQ = (if null entered then ifQ else drop 1 ifQ) <> started
        , idQ = drop (count idIssue) idQ <> entered
        , rnQ = move (count idIssue) idQ (count rnIssue) rnQ
        , rrQ = move (count rnIssue) rnQ (count rrIssue) rrQ
        , exQ = move (count rrIssue) rrQ (count exIssue) exQ
        , maQ = move (count exIssue) exQ (count maIssue) maQ
        , cmQ = move (count maIssue) maQ retires cmQ
        }

count :: (Integral a) => a -> Int
count = fromIntegral

stages :: Model -> [(Inflight, Stage)]
stages Model {..} = concat [slot IF (concat ifQ), slot ID idQ, slot RN rnQ, slot RR rrQ, slot EX exQ, slot MA maQ, slot Cm cmQ]
  where
    slot s is = [(i, s) | i <- is]

label :: Addr -> Maybe Inst -> String
label pc bits = printf "%s: %s" (hex pc) (maybe "(not fetched)" hex bits)

clockLines :: CoreTrace -> [(Inflight, Stage)] -> Model -> [String]
clockLines trace@CoreTrace {..} was cur@Model {..} = concatMap entering (stages cur) <> retiredLog <> concatMap lostLines lost
  where
    seen = [(i.instId, s) | (i, s) <- was]
    entering (i, s) = case lookup i.instId seen of
      Just u | u == s -> []
      Just _ -> [sLine i s]
      Nothing -> [printf "I\t%d\t%d\t0" i.instId i.instId, sLine i s]

    sLine i s = printf "S\t%d\t0\t%s" i.instId (show s)

    retiredLog = concat (zipWith3 ofRetire [0 ..] cmQ (catMaybes (toList retired)))
      where
        ofRetire k i l =
          printf "L\t%d\t0\t%s" i.instId (label l.pc (Just l.instBits))
            : [printf "L\t%d\t1\t%s" i.instId ln | ln <- retireLines l]
              <> [printf "R\t%d\t%d\t0" i.instId (commits + k)]

    -- a predicted-taken branch cuts off the rest of its fetch; EX drops the lanes younger than a redirect or a trap; a flush drops everything still upstream of MA
    lost = snd (handedOver cur trace) <> squashed <> flushedOut
      where
        squashed = if flush || count exIssue > 0 then drop (count exIssue) exQ else []
        flushedOut = if flush then rrQ <> rnQ <> idQ <> concat ifQ else []

    lostLines i = [printf "L\t%d\t0\t%s" i.instId (label i.pc i.instBits), printf "R\t%d\t%d\t1" i.instId i.instId]

konataLog :: [CoreTrace] -> [String]
konataLog ts = "Kanata\t0004" : "C=\t0" : concat (zipWith3 clock ts ([] : map stages models) models)
  where
    models = scanl modelStep initModel ts
    clock t was cur = clockLines t was cur <> ["C\t1"]
