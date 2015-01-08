{-# LANGUAGE QuasiQuotes, ScopedTypeVariables, ViewPatterns, NamedFieldPuns #-}

module Gnuplot where


import           Data.Maybe (fromMaybe)
import           Data.List
import           Data.String.Interpolate
import           Data.Time
import           System.Locale


data Run = Run {
    runTime :: (UTCTime, UTCTime)
  , runHost :: String
  , runName :: String
  } deriving (Show, Eq, Ord)

ytics :: [(Integer, Run)] -> String
ytics runs = intercalate ", " $ for runs $
  \ (ix, Run {runHost, runName}) ->
    [i|"#{runName} - #{runHost}" #{ix}|]

objects :: [(Integer, Run)] -> String
objects runs = unlines $ for runs $ \(id, r) -> object_ (getColor r) id r
  where
    getColor = fromMaybe "#8888FF" . flip lookup colors . runHost
    colors = zip uniqueHosts $
      cycle ["#163A07", "#CA7ADE", "#B0C4EE", "#719D71", "#4BA681", "#597948", "#1028B0", "#6603AF", "#121BF8", "#D437AB", "#905099"]
    uniqueHosts = nub $ for runs $ runHost . snd

    object_ :: String -> Integer -> Run -> String
    object_ color (fromIntegral -> ix :: Double) (Run {runTime}) =
      -- set object 1 rectangle from "2015-01-05_00:00:00", 2.6 to "2015-01-05_04:03:00", 3.4 fillcolor rgb "#8888ff" fillstyle solid 0.8
      [i|set object #{show ix} rectangle from #{showTime (fst runTime)}, #{ix - 0.5} to #{showTime (snd runTime)}, #{ix + 0.5} fillcolor rgb "#{color}" fillstyle solid 0.8|]

showTime :: UTCTime -> String
showTime = formatTime defaultTimeLocale "\"%Y-%m-%d_%H:%M:%S\"" . utcToLocalTime (TimeZone (60 * 8) False "SGT")

for :: [a] -> (a -> b) -> [b]
for = flip map
