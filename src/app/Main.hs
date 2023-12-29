import Data.Time.LocalTime (TimeOfDay (..), getCurrentTimeZone)
import System.Exit (exitFailure, exitSuccess)
import Text.Printf (printf)

import PbMetar.Common
import PbMetar.Curl (getMetar)
import PbMetar.Log (Priority (NOTICE), criticalM, infoM, initLogging, lname, noticeM, out)
import PbMetar.Math
import PbMetar.Metar (isolateMetarLine, parse)
import PbMetar.Opts (parseOpts)


main :: IO ()
main = do
  opts <- parseOpts
  initLogging NOTICE
  infoM lname "polybar-metar-weather started"
  curlResult <- getMetar $ optStation opts
  let eMetarString = isolateMetarLine =<< curlResult
  either (const $ pure ()) (noticeM lname) eMetarString
  tz <- getCurrentTimeZone
  let parsed = parse tz =<< eMetarString
  either exitFail exitOk parsed


exitOk :: Metar -> IO ()
exitOk (Metar (TimeOfDay h m _) windKts tc@(TempCelsius tempC)) = do
  let tempFahr@(TempFahr tempF) = celsiusToFahrenheit tc
  let windMph@(WindMph windM) = knotsToMph windKts
  let wcF@(TempFahr windChillF) = calculateWindChill windMph tempFahr
  let (TempCelsius windChillC) = fahrenheitToCelsius wcF
  out $ printf "%%{T2}\xe586%%{T-} %02d:%02d %%{T2}\xf2c9%%{T-} %.0f°F %01f°C %%{T2}\xf72e%%{T-} %.1fmph %.0f°F %.1f°C"
    h m tempF tempC windM windChillF windChillC
  infoM lname "polybar-metar-weather finished successfully"
  exitSuccess


exitFail :: String -> IO ()
exitFail errorInfo = do
  criticalM lname $ printf "Error: %s" errorInfo
  out "ERROR! See log for more info"
  exitFailure
