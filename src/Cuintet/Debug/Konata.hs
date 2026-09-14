{-# LANGUAGE StrictData #-}

-- | A Konata pipeline log, reconstructed from the per-clock 'CoreTrace'.
module Cuintet.Debug.Konata (konataLog) where

import Clash.Prelude (imap, natToNum)
import Cuintet.Core (CoreTrace (..))
import Cuintet.CoreCtrl (ExecUnit (..), usesRs1, usesRs2)
import Cuintet.Debug.Show (hex, retireLines)
import Cuintet.Eei (Addr, FetchWidth, Inst, PRegAddr, RobAddr)
import Cuintet.Pipeline (Fetched (..), Renamed (..), Retire (..))
import Cuintet.Unit.Btb (bankOf)
import Data.Foldable (toList)
import Data.Function (applyWhen)
import Data.List (mapAccumL, partition)
import Data.Maybe (catMaybes, fromMaybe)
import Text.Printf (printf)
import Prelude

data Inflight = Inflight
  { instId :: Int
  , pc :: Addr
  , instBits :: Maybe Inst
  }

data Stage = IF | ID | RN | IQ | RR | EX | MD | LS | WB | Cm
  deriving (Eq, Show)

unitStage :: ExecUnit -> Stage
unitStage = \case
  MulDivUnit -> MD
  MemUnit -> LS

data Tracked = Tracked
  { robAddr :: RobAddr
  , pdAddr :: Maybe PRegAddr
  , inflight :: Inflight
  , completed :: Bool
  }

-- | Where everything in flight is at the start of a clock. Up to RN a fetch moves as a group; from the ROB on an entry is found by its address.
data Model = Model
  { nextId :: Int
  , commits :: Int
  , ifQ :: [[Inflight]]
  , idQ :: [Inflight]
  , rnQ :: [Inflight]
  , rob :: [Tracked]
  , drawn :: [(Int, Stage)]
  }

initModel :: Model
initModel = Model {nextId = 0, commits = 0, ifQ = [], idQ = [], rnQ = [], rob = [], drawn = []}

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
    entries = catMaybes (toList ifIssue)
    fill i e
      | i.pc == e.pc = Inflight {instId = i.instId, pc = e.pc, instBits = Just e.instBits}
      | otherwise = errorWithoutStackTrace "konataLog: lost track of the fetch path"

-- | A group leaves a stage whole, so a stage holds either what the one upstream just handed it or what it already had.
move :: Int -> [Inflight] -> Int -> [Inflight] -> [Inflight]
move push src pop cur
  | push > 0 = take push src
  | pop > 0 = []
  | otherwise = cur

count :: (Integral a) => a -> Int
count = fromIntegral

label :: (Addr -> String) -> Addr -> Maybe Inst -> String
label mnemonic pc bits = printf "%s: %s  %s" (hex pc) (maybe "(not fetched)" hex bits) (mnemonic pc)

lostLines :: (Addr -> String) -> Inflight -> [String]
lostLines mnemonic i = [printf "L\t%d\t0\t%s" i.instId (label mnemonic i.pc i.instBits), printf "R\t%d\t%d\t1" i.instId i.instId]

-- | The stages of one clock, then its retires and the instructions it drops, which end at the next.
clock :: (Addr -> String) -> Model -> CoreTrace -> (Model, [String])
clock mnemonic model@Model {..} trace@CoreTrace {..} =
  (model', concatMap (lostLines mnemonic) squashed <> concatMap draw stages <> depLines <> ["C\t1"] <> concat (zipWith retireLog [commits ..] retires) <> concatMap (lostLines mnemonic) lost)
  where
    -- an entry gone from the ROB without retiring was squashed at the end of the last clock
    (live, gone) = partition (\t -> t.robAddr - robHead < robTail - robHead) rob
    squashed
      | length live /= count (robTail - robHead) = errorWithoutStackTrace "konataLog: lost track of the ROB"
      | otherwise = (.inflight) <$> gone

    holders =
      [(a, RR) | Just a <- toList rrIssue]
        <> [(a, EX) | Just a <- toList exHold]
        <> [(a, unitStage (toEnum (fromEnum u))) | (u, Just a) <- toList (imap (,) unitHold)]
        <> [(a, WB) | Just a <- toList wbHold]
    stageOf t = fromMaybe (if t.completed then Cm else IQ) (lookup t.robAddr holders)

    stages =
      [(i, IF) | i <- concat ifQ]
        <> [(i, ID) | i <- idQ]
        <> [(i, RN) | i <- rnQ]
        <> [(t.inflight, stageOf t) | t <- live]

    draw (i, s) = case lookup i.instId drawn of
      Just u | u == s -> []
      Just _ -> [sLine]
      Nothing -> [printf "I\t%d\t%d\t0" i.instId i.instId, sLine]
      where
        sLine = printf "S\t%d\t0\t%s" i.instId (show s)

    retires = [(robHead + k, r) | (k, Just r) <- zip [0 ..] (toList cmRetire)]
    retireLog n (a, r) = case [t.inflight | t <- live, t.robAddr == a] of
      [i]
        | i.pc == r.pc ->
            printf "L\t%d\t0\t%s" i.instId (label mnemonic r.pc (Just r.instBits))
              : [printf "L\t%d\t1\t%s" i.instId ln | ln <- retireLines r]
                <> [printf "R\t%d\t%d\t0" i.instId n]
      _ -> errorWithoutStackTrace "konataLog: retired an entry it was not tracking"

    (entered, cutOff) = handedOver model trace
    started = maybe [] (\addr -> [fetchGroup nextId addr]) ifStart
    ifQ' = (if null entered then ifQ else drop 1 ifQ) <> started
    idQ' = drop (count idIssue) idQ <> entered
    renamed = catMaybes (toList rnIssue)
    rnQ' = move (count idIssue) idQ (length renamed) rnQ

    allocated
      | not (null renamed) && length renamed /= length rnQ = errorWithoutStackTrace "konataLog: RN renamed part of a group"
      | otherwise = zipWith (\r i -> Tracked {robAddr = r.robAddr, pdAddr = r.pdAddr, inflight = i, completed = False}) renamed rnQ

    -- a source waits on the live entry its physical register is allocated to
    depLines =
      [ printf "W\t%d\t%d\t0" i.instId t.inflight.instId
      | (r, i) <- zip renamed rnQ
      , (uses, ps) <- [(usesRs1 r.ctrl, r.ps1Addr), (usesRs2 r.ctrl, r.ps2Addr)]
      , uses
      , t <- live
      , t.pdAddr == Just ps
      ]
    done = catMaybes (toList wbComplete)
    rob' =
      [t {completed = t.completed || t.robAddr `elem` done} | t <- live, t.robAddr `notElem` map fst retires]
        <> allocated

    lost = cutOff <> if flush then concat ifQ' <> idQ' <> rnQ' else []

    model' =
      Model
        { nextId = nextId + length (concat started)
        , commits = commits + length retires
        , ifQ = applyWhen flush (const []) ifQ'
        , idQ = applyWhen flush (const []) idQ'
        , rnQ = applyWhen flush (const []) rnQ'
        , rob = rob'
        , drawn = [(i.instId, s) | (i, s) <- stages]
        }

konataLog :: (Addr -> String) -> [CoreTrace] -> [String]
konataLog mnemonic ts = "Kanata\t0004" : "C=\t0" : concat (snd (mapAccumL (clock mnemonic) initModel ts))
