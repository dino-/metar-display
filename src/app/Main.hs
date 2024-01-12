import Data.Time.LocalTime (getCurrentTimeZone)
import System.Exit (exitFailure, exitSuccess)

import PbMetar.Curl (getMetar)
import PbMetar.Log (criticalM, infoM, initLogging, lname, noticeM, out)
import PbMetar.Metar (isolateMetarLine, parse)
import PbMetar.Model.Options (Options (..))
import PbMetar.Opts (parseOpts)
import PbMetar.Output (mkOutput)


main :: IO ()
main = do
  opts <- parseOpts
  initLogging $ optVerbosity opts
  infoM lname "polybar-metar-weather started"
  curlResult <- getMetar $ optStation opts
  let eMetarString = isolateMetarLine =<< curlResult
  either (const $ pure ()) (noticeM lname) eMetarString
  let parsed = parse =<< eMetarString
  localZone <- getCurrentTimeZone
  either exitFail exitOk $ mkOutput (optTemplate opts) localZone =<< parsed


exitOk :: String -> IO ()
exitOk polybarLabel = do
  infoM lname $ "stdout output string: " <> polybarLabel
  out polybarLabel
  infoM lname "polybar-metar-weather finished successfully"
  exitSuccess


exitFail :: String -> IO ()
exitFail errorInfo = do
  criticalM lname $ "Error: " <> errorInfo
  out "ERROR! See log for more info"
  exitFailure
