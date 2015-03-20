{-# LANGUAGE QuasiQuotes #-}

module RunSpec where


import           Data.String.Interpolate
import           Data.String.Interpolate.Util
import           Data.Time
import           System.Exit
import           System.IO
import           System.IO.Temp
import           System.Locale
import           System.Process
import           Test.Hspec

import           Gnuplot
import           Run


spec :: Spec
spec = do
  describe "parseLogTime" $ do
    it "parses the time" $ do
      let s = "Jan 5 07:17:04"
      formatTime defaultTimeLocale "%F_%T" (parseLogTime s) `shouldBe` "1970-01-05_07:17:04"

  describe "parseLines" $ do
    it "parses multiple syslog lines" $ do
      parseLines logLines `shouldBe` expectedLines

  describe "aggregateRuns" $ do
    it "aggregates runs" $ do
      aggregateRuns expectedLines `shouldBe` expectedRuns

  describe "filterRuns" $ do
    it "removes runs that start and end with the same time" $ do
      filterRuns expectedRuns `shouldBe`
        Run (parseLogTime "Jan  5 07:17:04", parseLogTime "Jan  5 07:30:50") "ip-111-22-3-44" "huhu" :
        []

  describe "printGnuplot" $ do
    it "can be parsed by gnuplot" $ withSystemTempFile "cron-syslog-viewer-test" $ \ file handle -> do
      hPutStrLn handle (printGnuplot expectedRuns)
      hClose handle
      system ("gnuplot " ++ file) `shouldReturn` ExitSuccess


expectedRuns :: [Run]
expectedRuns =
  Run (parseLogTime "Jan  5 07:17:01", parseLogTime "Jan  5 07:17:01") "ip-111-22-3-44" "foo bar" :
  Run (parseLogTime "Jan  5 07:17:04", parseLogTime "Jan  5 07:30:50") "ip-111-22-3-44" "huhu" :
  Run (parseLogTime "Jan  5 07:20:01", parseLogTime "Jan  5 07:20:02") "ip-111-22-3-44" "test bumm" :
  []


expectedLines :: [Line]
expectedLines =
  Line (parseLogTime "Jan  5 07:17:01") Start (Job "ip-111-22-3-44" "foo bar" 17399) :
  Line (parseLogTime "Jan  5 07:17:01") End   (Job "ip-111-22-3-44" "foo bar" 17398) :
  Line (parseLogTime "Jan  5 07:17:04") Start (Job "ip-111-22-3-44" "huhu" 17397) :
  Line (parseLogTime "Jan  5 07:20:01") Start (Job "ip-111-22-3-44" "test bumm" 17417) :
  Line (parseLogTime "Jan  5 07:20:02") End   (Job "ip-111-22-3-44" "test bumm" 17416) :
  Line (parseLogTime "Jan  5 07:30:50") End   (Job "ip-111-22-3-44" "huhu" 17396) :
  []

logLines :: String
logLines = unindent [i|
  <13> Jan  5 07:17:01 ip-111-22-3-44 CRON[17399]: info: (root) CMD foo bar
  <13> Jan  5 07:17:01 ip-111-22-3-44 CRON[17398]: info: (root) END foo bar
  <13> Jan  5 07:17:04 ip-111-22-3-44 CRON[17397]: info: (root) CMD huhu
  <13> Jan  5 07:18:06 ip-111-22-3-44 dhclient: info: DHCPREQUEST of [snip]
  <13> Jan  5 07:18:06 ip-111-22-3-44 dhclient: info: DHCPACK of [snip]
  <13> Jan  5 07:18:06 ip-111-22-3-44 dhclient: info: bound to [snip]
  <13> Jan  5 07:20:01 ip-111-22-3-44 CRON[17417]: info: (smmsp) CMD test bumm
  <13> Jan  5 07:20:02 ip-111-22-3-44 CRON[17416]: info: (smmsp) END test bumm
  <13> Jan  5 07:30:24 ip-111-22-3-44 sm-mta[8739]: notice: rejecting connections on daemon MTA-v4: load average: 12
  <13> Jan  5 07:30:24 ip-111-22-3-44 sm-mta[8739]: notice: rejecting connections on daemon MSP-v4: load average: 12
  <13> Jan  5 07:30:39 ip-111-22-3-44 sm-mta[8739]: notice: rejecting connections on daemon MTA-v4: load average: 16
  <13> Jan  5 07:30:39 ip-111-22-3-44 sm-mta[8739]: notice: rejecting connections on daemon MSP-v4: load average: 16
  <13> Jan  5 07:30:50 ip-111-22-3-44 CRON[17396]: info: (root) END huhu
  <13> Jan  5 07:30:54 ip-111-22-3-44 sm-mta[8739]: notice: rejecting connections on daemon MTA-v4: load average: 19
  <13> Jan  5 07:30:54 ip-111-22-3-44 sm-mta[8739]: notice: rejecting connections on daemon MSP-v4: load average: 19
  <13> Jan  5 07:31:09 ip-111-22-3-44 sm-mta[8739]: notice: rejecting connections on daemon MTA-v4: load average: 21
 |]
