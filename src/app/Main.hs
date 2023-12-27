import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)

import PbMetar.Curl (getMetar)
import PbMetar.Metar (parse)
import PbMetar.Types


main :: IO ()
main = do
  curlResult <- getMetar
  let parsed = parse =<< curlResult
  either exitFail exitOk parsed


exitOk :: Metar -> IO ()
exitOk (Metar (Time h m) windKts tc@(TempCelsius tempC)) = do
  let tempFahr@(TempFahr tempF) = celsiusToFahrenheit tc
  let windMph@(WindMph windM) = knotsToMph windKts
  let wcF@(TempFahr windChillF) = calculateWindChill windMph tempFahr
  let (TempCelsius windChillC) = fahrenheitToCelsius wcF
  printf "%%{T2}\xe586%%{T-} %02d:%02d %%{T2}\xf2c9%%{T-} %.0f°F %01f°C %%{T2}\xf72e%%{T-} %.1fmph %.0f°F %.1f°C\n"
    h m tempF tempC windM windChillF windChillC
  exitSuccess


exitFail :: String -> IO ()
exitFail errorInfo = do
  hPutStrLn stderr $ printf "Error: %s" errorInfo
  exitFailure


celsiusToFahrenheit :: TempCelsius -> TempFahr
celsiusToFahrenheit (TempCelsius tempCelsius) = TempFahr $ tempCelsius * (9 / 5) + 32.0


fahrenheitToCelsius :: TempFahr -> TempCelsius
fahrenheitToCelsius (TempFahr tempFahr) = TempCelsius $ (tempFahr - 32) * (5 / 9)


knotsToMph :: WindKts -> WindMph
knotsToMph (WindKts knots) = WindMph $ knots * 6076 / 5280


calculateWindChill :: WindMph -> TempFahr -> TempFahr
calculateWindChill (WindMph wind) (TempFahr tempF) = TempFahr $
  34.74 + (0.6215 * tempF) - (35.75 * (wind ** exp')) + (0.4275 * tempF * (wind ** exp'))
  where exp' = 0.16 :: Double
