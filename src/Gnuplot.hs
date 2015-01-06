{-# LANGUAGE QuasiQuotes #-}

module Gnuplot where


import           Data.List
import           Data.String.Interpolate
import           Data.Time
import           System.Locale


data Run = Run (UTCTime, UTCTime) String
  deriving (Show, Eq, Ord)

ytics :: [(Integer, Run)] -> String
ytics runs = intercalate ", " $ for runs $
  \ (ix, Run _ name) ->
    [i|"#{name}" #{ix}|]

objects :: [(Integer, Run)] -> String
objects runs = unlines $ for runs $
 \ (ix, Run (start, end) _name) ->
    -- set object 1 rectangle from "2015-01-05_00:00:00", 2.6 to "2015-01-05_04:03:00", 3.4 fillcolor rgb "#8888ff" fillstyle solid 0.8
    [i|set object #{show ix} rectangle from #{showTime start}, #{ix - 1} to #{showTime end}, #{ix} fillcolor rgb "#8888ff" fillstyle solid 0.8|]

showTime :: UTCTime -> String
showTime = formatTime defaultTimeLocale "\"%Y-%m-%d_%H:%M:%S\""

for :: [a] -> (a -> b) -> [b]
for = flip map
