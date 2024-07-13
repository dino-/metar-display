import Data.Time.LocalTime (getCurrentTimeZone)
import System.Exit (exitFailure, exitSuccess)

import MetarDisplay.Curl (getMetar)
import MetarDisplay.Log (criticalM, infoM, initLogging, lname, noticeM, out)
import MetarDisplay.Metar (isolateMetarLine, parse)
import MetarDisplay.Model.Options (Options (..))
import MetarDisplay.Opts (parseOpts)
import MetarDisplay.Output (mkOutput)


main :: IO ()
main = do
  opts <- parseOpts
  initLogging (optLogDate opts) (optVerbosity opts)
  infoM lname "metar-display started"
  curlResult <- getMetar $ optStation opts
  let eMetarString = isolateMetarLine =<< curlResult
  either (const $ pure ()) (noticeM lname) eMetarString
  let parsed = parse =<< eMetarString
  localZone <- getCurrentTimeZone
  either exitFail exitOk $ mkOutput (optTemplate opts) localZone =<< parsed


exitOk :: String -> IO ()
exitOk output = do
  infoM lname $ "stdout output string: " <> output
  out output
  infoM lname "metar-display finished successfully"
  exitSuccess


exitFail :: String -> IO ()
exitFail errorInfo = do
  criticalM lname $ "Error: " <> errorInfo
  out "ERROR! See log for more info"
  exitFailure
