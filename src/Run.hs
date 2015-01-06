{-# LANGUAGE QuasiQuotes, ViewPatterns #-}

module Run where


import           Control.Arrow
import           Data.List                    as List (foldl', isPrefixOf,
                                                       isInfixOf, sort)
import           Data.Map                     as Map hiding (filter, map)
import           Data.Maybe
import           Data.String.Interpolate
import           Data.String.Interpolate.Util
import           Data.Time
import           Safe
import           System.Locale

import           Gnuplot


run :: String -> String
run =
  parseLines >>>
  sort >>>
  aggregateRuns >>>
  filterRuns >>>
  printGnuplot


data Line
  = Line UTCTime CronMarker Job
 deriving (Show, Eq, Ord)

data CronMarker = Start | End
  deriving (Show, Eq, Ord)

data Job = Job String String Integer
  deriving (Show, Eq, Ord)

parseLines :: String -> [Line]
parseLines = catMaybes . map parseLine . lines

parseLine :: String -> Maybe Line
parseLine (words -> (month : day : time : host : job : _logLevel : _ : cronMarker : command)) = do
  marker <- case cronMarker of
    "CMD" -> Just Start
    "END" -> Just End
    _ -> Nothing
  Just $ Line (parseLogTime (unwords [month, day, time])) marker (Job host (snip (unwords command)) (parsePID job))
parseLine _ = Nothing

snip :: String -> String
snip s = if "2>" `isPrefixOf` s
  then ""
  else case s of
    (a : r) -> a : snip r
    "" -> ""

-- years are not available, assuming 1970
parseLogTime :: String -> UTCTime
parseLogTime s =
  fromMaybe (error ("cannot parse: " ++ s)) $
  parseTime defaultTimeLocale "%b %e %T" s

parsePID :: String -> Integer
parsePID s = case dropWhile (/= '[') s of
  ('[' : rest) -> readNote err $ takeWhile (/= ']') rest
  _ -> error err
 where
  err = "cannot parse job: " ++ s


aggregateRuns :: [Line] -> [Run]
aggregateRuns = sort . snd . List.foldl' inner (empty, [])
  where
    inner :: (Map Job UTCTime, [Run]) -> Line -> (Map Job UTCTime, [Run])
    inner (starts, runs) (Line time Start job) = (insert job time starts, runs)
    inner (starts, runs) (Line end End (Job host name pid)) =
      let newRuns = case Map.lookup (Job host name (succ pid)) starts of
            Nothing -> runs
            Just start -> Run (start, end) host name : runs
      in (delete (Job host name (succ pid)) starts, newRuns)


filterRuns :: [Run] -> [Run]
filterRuns = filter $ \ (Run (start, end) _host name) ->
  diffUTCTime end start > 1 &&
  not ("cron-msp" `isInfixOf` name)


printGnuplot :: [Run] -> String
printGnuplot (zip [1 :: Integer ..] -> runs) = unindent [i|

  set term png size 1600,900
  set output "test.png"

  set xdata time
  set timefmt "%Y-%m-%d_%H:%M:%S"

  set xrange [#{showTime (minimum (for runs (\ (_, Run (start, _) _ _) -> start)))}:#{showTime (maximum (for runs (\ (_, Run (_, end) _ _) -> end)))}]
  set yrange [0.4:#{fromIntegral (maximum (for runs (\ (ix, _) -> ix))) + 0.6 :: Double}]
  set xlabel "time"
  set ylabel ""
  set title "cron job times"
  set ytics (#{ytics runs})
  set key outside width +2
  set grid xtics
  set palette model RGB
  unset colorbox
  #{objects runs}

  plot -1

 |]
