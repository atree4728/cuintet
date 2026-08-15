module Main (main) where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON (parseJSON), eitherDecodeFileStrict', withObject, withText, (.!=), (.:), (.:?))
import Data.List (find, sortOn, unsnoc)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Ord (Down (Down))
import Data.Text (Text)
import Data.Text qualified as T
import Prettyprinter
import Prettyprinter.Render.Text (renderIO)
import System.Directory (listDirectory)
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath (takeBaseName, takeExtension, (</>))
import System.IO (stdout)
import Text.Printf (printf)

-- | @report.json@, cut down to what is printed.
data Report = Report
  { fmax :: Map Text Fmax
  , criticalPaths :: [Path]
  , netTimings :: [NetTiming]
  -- ^ Empty unless nextpnr was given @--detailed-timing-report@.
  }

data Fmax = Fmax {achieved :: Double, constraint :: Double}

data Path = Path {from :: Text, to :: Text, segments :: [Segment]}

data Segment = Segment
  { delay :: Double
  , kind :: Kind
  , net :: Maybe Text
  , from :: Maybe End
  , to :: Maybe End
  }

-- | One end of a segment: where in the fabric it is, and in which cell.
data End = End {cell :: Maybe Text, loc :: Maybe Loc}

-- | A net and when its value lands at each cell it drives.
data NetTiming = NetTiming {net :: Text, endpoints :: [Endpoint]}

-- | Where a net lands, and how long after the clock edge it gets there.
data Endpoint = Endpoint {cell :: Text, arrival :: Double}

data Loc = Loc Int Int
  deriving (Eq)

-- | What a step of a path spends its time on.
data Kind = ClkToQ | Logic | Routing | Source | Setup
  deriving (Eq, Ord)

instance FromJSON Report where
  parseJSON = withObject "report" $ \o ->
    Report <$> o .: "fmax" <*> o .: "critical_paths" <*> o .:? "detailed_net_timings" .!= []

instance FromJSON Fmax where
  parseJSON = withObject "fmax" $ \o ->
    Fmax <$> o .: "achieved" <*> o .: "constraint"

instance FromJSON Path where
  parseJSON = withObject "critical path" $ \o ->
    Path <$> o .: "from" <*> o .: "to" <*> o .: "path"

instance FromJSON Segment where
  parseJSON = withObject "segment" $ \o ->
    Segment
      <$> o .: "delay"
      <*> o .: "type"
      <*> o .:? "net"
      <*> o .:? "from"
      <*> o .:? "to"

instance FromJSON End where
  parseJSON = withObject "end" $ \o -> End <$> o .:? "cell" <*> o .:? "loc"

instance FromJSON NetTiming where
  parseJSON = withObject "net timing" $ \o ->
    NetTiming <$> o .: "net" <*> o .: "endpoints"

-- | @delay@ is the routing delay and the arrival time; only the latter is used.
instance FromJSON Endpoint where
  parseJSON = withObject "endpoint" $ \o -> do
    cell <- o .: "cell"
    o .: "delay" >>= \case
      [_, arrival] -> pure Endpoint {cell, arrival}
      xs -> fail ("delay of " <> show (length (xs :: [Double])) <> " entries")

instance FromJSON Loc where
  parseJSON v =
    parseJSON v >>= \case
      [x, y] -> pure (Loc x y)
      xs -> fail ("loc of " <> show (length (xs :: [Int])) <> " coordinates")

instance FromJSON Kind where
  parseJSON = withText "type" $ \t -> case t of
    "clk-to-q" -> pure ClkToQ
    "logic" -> pure Logic
    "routing" -> pure Routing
    "source" -> pure Source
    "setup" -> pure Setup
    _ -> fail ("unknown segment type " <> T.unpack t)

-- * Attributing a path to the units it runs through

-- | A Clash binding that became its own HDL module, so its cells stay tellable.
newtype Unit = Unit Text
  deriving (Eq, Ord)

-- | One step of a path: a cell's own delay, or the wire between two cells.
data Hop = Hop
  { delay :: Double
  , kind :: Kind
  , src :: Unit
  , dst :: Unit
  , srcLoc :: Maybe Loc
  , dstLoc :: Maybe Loc
  , name :: Text
  -- ^ the net for a routing hop, the cell otherwise
  }

-- | A critical path with its hops attributed and its delay added up.
data Walk = Walk {route :: Text, total :: Double, hops :: [Hop]}

-- | What Clash emitted, longest first so one module name cannot shadow another.
modules :: FilePath -> IO [Text]
modules dir = do
  files <- listDirectory dir
  let names =
        [ name
        | f <- files
        , takeExtension f == ".sv"
        , let name = T.pack (takeBaseName f)
        , not ("_types" `T.isSuffixOf` name)
        ]
  pure (sortOn (Down . T.length) names)

{- | The unit a flattened cell came from, read off the instance path yosys kept.

The path nests, so the innermost match wins: a cell of the ALU is reported as
the ALU rather than as the EX stage it sits in.
-}
unitOf :: [Text] -> Text -> Unit
unitOf mods cell = foldl step (Unit "(top)") (maybe [] fst (unsnoc (T.splitOn "." cell)))
  where
    step found inst = maybe found (Unit . T.takeWhileEnd (/= '_')) (find (encloses inst) mods)
    encloses inst m = inst == m || (m <> "_") `T.isPrefixOf` inst

walk :: [Text] -> Path -> Walk
walk mods p =
  Walk
    { route = domain p.from <> " -> " <> domain p.to
    , total = sum [h.delay | h <- hs]
    , hops = hs
    }
  where
    hs = map hop p.segments
    hop seg =
      Hop
        { delay = seg.delay
        , kind = seg.kind
        , src = unitOf mods (fromMaybe "" (cellOf seg.from <|> cellOf seg.to))
        , dst = unitOf mods (fromMaybe "" (cellOf seg.to <|> cellOf seg.from))
        , srcLoc = seg.from >>= (.loc)
        , dstLoc = seg.to >>= (.loc)
        , name = fromMaybe "" (seg.net <|> cellOf seg.to <|> cellOf seg.from)
        }
    cellOf end = end >>= (.cell)

-- | Add up the hops under whatever they have in common, heaviest first.
tally :: (Ord k) => (Hop -> k) -> [Hop] -> [(k, Double)]
tally key = sortOn (Down . snd) . Map.toList . Map.fromListWith (+) . map (\h -> (key h, h.delay))

-- | The latest endpoint of every unit, which is where that unit's longest path ends.
latest :: [Text] -> [NetTiming] -> [(Unit, (Double, Text))]
latest mods nts =
  sortOn (Down . fst . snd) . Map.toList $
    Map.fromListWith
      (\a b -> if fst a >= fst b then a else b)
      [ (unitOf mods e.cell, (e.arrival, nt.net))
      | nt <- nts
      , e <- nt.endpoints
      ]

-- * Printing

-- | Columns of the widest bar, and the width a cell or net name is cut to.
barWidth, nameWidth :: Int
barWidth = 24
nameWidth = 44

render :: [Text] -> Report -> Doc ann
render mods report = vcat (map (uncurry clock) (Map.toList report.fmax) <> body <> units)
  where
    body = case sortOn (Down . (.total)) (map (walk mods) report.criticalPaths) of
      [] -> []
      w : rest -> ["", headline w, detail w] <> also rest
    also [] = []
    also ws = ["", "also"] <> map minor ws

    -- what each unit would hold the clock to on its own, once the ones above it are gone
    units = case latest mods report.netTimings of
      [] -> []
      us -> ["", "longest path ending in each unit"] <> map unitLimit us

clock :: Text -> Fmax -> Doc ann
clock name f =
  hcat $
    punctuate
      "   "
      [ pretty (domain name)
      , num "%.2f MHz" f.achieved
      , "period" <+> num "%.2f ns" (1000 / f.achieved)
      , parens (num "target %.0f MHz" f.constraint)
      ]

headline :: Walk -> Doc ann
headline w =
  hcat $
    punctuate
      "   "
      [ "critical path"
      , pretty w.route
      , num "%.2f ns" w.total
          <+> pretty (printf "over %d hops, %d unit crossings" (length w.hops) crossings :: String)
      ]
  where
    crossings = length [() | h <- w.hops, h.src /= h.dst]

-- | A path that is not the critical one; only its ends and its length matter.
minor :: Walk -> Doc ann
minor w =
  "  "
    <> fill 34 (pretty w.route)
    <> num "%8.2f ns" w.total
    <+> pretty (printf "over %d hops" (length w.hops) :: String)

detail :: Walk -> Doc ann
detail w =
  vcat $
    map kindRow (tally (.kind) w.hops)
      <> ("" : map unitRow (filter ((> 0) . snd) (tally (.src) w.hops)))
      <> ["", "  slowest hops"]
      <> map hopRow (take 8 (sortOn (Down . (.delay)) w.hops))
  where
    share d = d / w.total
    kindRow (k, d) = "  " <> fill 10 (pretty (kindName k)) <> amount d
    unitRow (u, d) = "  " <> fill 10 (unit u) <> amount d <> "  " <> bar (share d)
    amount d =
      num "%8.2f ns" d
        <+> pretty (printf "%3.0f%%" (100 * share d) :: String)
    bar r = pretty (T.replicate (round (r * fromIntegral barWidth)) "#")
    hopRow h =
      "    "
        <> num "%5.2f ns" h.delay
        <> "  "
        <> fill 9 (pretty (kindName h.kind))
        <> fill 32 (place h)
        <+> pretty (short h.name)

-- | One unit's own worst arrival, and the frequency that alone would allow.
unitLimit :: (Unit, (Double, Text)) -> Doc ann
unitLimit (u, (arrival, net)) =
  "  "
    <> fill 12 (unit u)
    <> num "%8.2f ns" arrival
    <+> num "%7.1f MHz" (1000 / arrival)
    <> "  "
    <> pretty (short net)

-- | Where a hop runs, named by unit and, when it moves, by fabric coordinates.
place :: Hop -> Doc ann
place h
  | h.dst /= h.src = unit h.src <+> at h.srcLoc <+> "->" <+> unit h.dst <+> at h.dstLoc
  | h.dstLoc /= h.srcLoc = unit h.src <+> at h.srcLoc <+> "->" <+> at h.dstLoc
  | otherwise = unit h.src <+> at h.srcLoc
  where
    at Nothing = ""
    at (Just (Loc x y)) = pretty (printf "(%d,%d)" x y :: String)

unit :: Unit -> Doc ann
unit (Unit u) = pretty u

kindName :: Kind -> Text
kindName = \case
  ClkToQ -> "clk-to-q"
  Logic -> "logic"
  Routing -> "routing"
  Source -> "source"
  Setup -> "setup"

num :: String -> Double -> Doc ann
num fmt x = pretty (printf fmt x :: String)

-- | Drop the decoration nextpnr puts on a clock net so the name is the signal.
domain :: Text -> Text
domain = T.replace "$glbnet$" "" . T.replace "$TRELLIS_IO_IN" ""

{- | Clash names run to hundreds of characters; only the head carries meaning.

The instance path is dropped because the unit column already says it.
-}
short :: Text -> Text
short (T.takeWhileEnd (/= '.') -> n)
  | T.length n <= nameWidth = n
  | otherwise = T.take (nameWidth - 1) n <> "~"

main :: IO ()
main = do
  (outDir, hdlDir) <-
    getArgs >>= \case
      [o, h] -> pure (o, h)
      _ -> die "usage: timing OUT_DIR HDL_DIR"
  report <- either die pure =<< eitherDecodeFileStrict' (outDir </> "report.json")
  mods <- modules hdlDir
  renderIO stdout $
    layoutPretty defaultLayoutOptions {layoutPageWidth = Unbounded} (render mods report <> hardline)
