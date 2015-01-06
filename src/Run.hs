{-# LANGUAGE QuasiQuotes, ViewPatterns #-}

module Run where


import           Control.Arrow
import           Data.List                    as List (foldl', 
                                                       sort)
import           Data.Map                     as Map hiding (map)
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
  aggregateRuns >>>
  printGnuplot


data Line
  = Line CronMarker UTCTime Job
 deriving (Show, Eq)

data CronMarker = Start | End
  deriving (Show, Eq)

data Job = Job String Integer
  deriving (Show, Eq, Ord)

parseLines :: String -> [Line]
parseLines = catMaybes . map parseLine . lines

parseLine :: String -> Maybe Line
parseLine (words -> (month : day : time : _host : job : _logLevel : _ : cronMarker : _)) = do
  marker <- case cronMarker of
    "CMD" -> Just Start
    "END" -> Just End
    _ -> Nothing
  Just $ Line marker (parseLogTime (unwords [month, day, time])) (parseJob job)
parseLine _ = Nothing

-- years are not available, assuming 1970
parseLogTime :: String -> UTCTime
parseLogTime s =
  fromMaybe (error ("cannot parse: " ++ s)) $
  parseTime defaultTimeLocale "%b %e %T" s

parseJob :: String -> Job
parseJob s = Job (takeWhile (/= '[') s) $ case dropWhile (/= '[') s of
  ('[' : rest) -> readNote err $ takeWhile (/= ']') rest
  _ -> error err
 where
  err = "cannot parse job: " ++ s


aggregateRuns :: [Line] -> [Run]
aggregateRuns = sort . snd . List.foldl' inner (empty, [])
  where
    inner :: (Map Job UTCTime, [Run]) -> Line -> (Map Job UTCTime, [Run])
    inner (starts, runs) (Line Start time job) = (insert job time starts, runs)
    inner (starts, runs) (Line End end (Job name pid)) =
      let newRuns = case Map.lookup (Job name (succ pid)) starts of
            Nothing -> runs
            Just start -> Run (start, end) name : runs
      in (delete (Job name (succ pid)) starts, newRuns)


printGnuplot :: [Run] -> String
printGnuplot (zip [1 :: Integer ..] -> runs) = unindent [i|

  set term png
  set output "test.png"

  set xdata time
  set timefmt "%Y-%m-%d_%H:%M:%S"

  set xrange [#{showTime (minimum (for runs (\ (_, Run (start, _) _) -> start)))}:#{showTime (maximum (for runs (\ (_, Run (_, end) _) -> end)))}]
  set yrange [1:#{maximum (for runs (\ (ix, _) -> ix))}]
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
