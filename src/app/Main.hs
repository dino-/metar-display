{-# LANGUAGE OverloadedRecordDot #-}

import Data.Time.LocalTime (TimeOfDay (..), getCurrentTimeZone)
import System.Exit (exitFailure, exitSuccess)
import Text.Printf (printf)

import PbMetar.Common
import PbMetar.Curl (getMetar)
import PbMetar.Log (Priority (NOTICE), criticalM, infoM, initLogging, lname, noticeM, out)
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
  either exitFail (exitOk $ optFontIndex opts) parsed


exitOk :: FontIndex -> Weather -> IO ()
exitOk fi@(FontIndex fontIndex) (Weather obs chill) = do
  let (TempFahr tempF) = obs.tempF
  let (TempCelsius tempC) = obs.tempC
  let (WindMph windMph) = obs.windMph
  let outputString = printf "%%{T%d}\xe586%%{T-} %02d:%02d %%{T%d}\xf2c9%%{T-} %.0f°F %01f°C %%{T%d}\xf72e%%{T-} %.1fmph%s"
        fontIndex obs.time.todHour obs.time.todMin
        fontIndex tempF tempC
        fontIndex windMph (mkWindChillDisplay fi chill)
  infoM lname $ "stdout output string: " <> outputString
  out outputString
  infoM lname "polybar-metar-weather finished successfully"
  exitSuccess


mkWindChillDisplay :: FontIndex -> WindChill -> String
mkWindChillDisplay (FontIndex fontIndex) (WindChill (TempCelsius windChillC) (TempFahr windChillF)) =
  printf " %%{T%d}\xf7ad%%{T-} %.0f°F %.1f°C" fontIndex windChillF windChillC
mkWindChillDisplay _ NoEffect = ""


exitFail :: String -> IO ()
exitFail errorInfo = do
  criticalM lname $ printf "Error: %s" errorInfo
  out "ERROR! See log for more info"
  exitFailure
