{-# LANGUAGE OverloadedRecordDot #-}

import Data.Time.LocalTime (TimeOfDay (..), getCurrentTimeZone)
import System.Exit (exitFailure, exitSuccess)
import Text.Printf (printf)

import PbMetar.Common
import PbMetar.Curl (getMetar)
import PbMetar.Log (criticalM, infoM, initLogging, lname, noticeM, out)
import PbMetar.Metar (isolateMetarLine, parse)
import PbMetar.Opts (parseOpts)


main :: IO ()
main = do
  opts <- parseOpts
  initLogging $ optVerbosity opts
  infoM lname "polybar-metar-weather started"
  curlResult <- getMetar $ optStation opts
  let eMetarString = isolateMetarLine =<< curlResult
  either (const $ pure ()) (noticeM lname) eMetarString
  tz <- getCurrentTimeZone
  let parsed = parse tz =<< eMetarString
  either exitFail (exitOk (optFontIndex opts) (optColorText opts)) parsed


exitOk :: FontIndex -> ColorText -> Weather -> IO ()
exitOk fi@(FontIndex fontIndex) colorText (Weather obs chill) = do
  let (TempFahr tempF) = obs.tempF
  let (TempCelsius tempC) = obs.tempC
  let (WindMph windMph) = obs.windMph
  let (colorBegin, colorEnd) = mkColorFormatting colorText
  let outputString = printf "%%{T%d}\xe586%%{T-} %s%02d:%02d%s %%{T%d}\xf2c9%%{T-} %s%.0f°F %01f°C%s %%{T%d}\xf72e%%{T-} %s%.1fmph%s%s"
        fontIndex colorBegin obs.time.todHour obs.time.todMin colorEnd
        fontIndex colorBegin tempF tempC colorEnd
        fontIndex colorBegin windMph colorEnd (mkWindChillDisplay fi colorText chill)
  infoM lname $ "stdout output string: " <> outputString
  out outputString
  infoM lname "polybar-metar-weather finished successfully"
  exitSuccess


mkWindChillDisplay :: FontIndex -> ColorText -> WindChill -> String
mkWindChillDisplay (FontIndex fontIndex) colorText (WindChill (TempCelsius windChillC) (TempFahr windChillF)) =
  printf " %%{T%d}\xf7ad%%{T-} %s%.0f°F %.1f°C%s" fontIndex colorBegin windChillF windChillC colorEnd
  where (colorBegin, colorEnd) = mkColorFormatting colorText
mkWindChillDisplay _ _ NoEffect = ""


mkColorFormatting :: ColorText -> (String, String)
mkColorFormatting NoColorChange = ("", "")
mkColorFormatting (ColorText colText) = ( printf "%%{F%s}" colText, "%%{F-}")


exitFail :: String -> IO ()
exitFail errorInfo = do
  criticalM lname $ printf "Error: %s" errorInfo
  out "ERROR! See log for more info"
  exitFailure
