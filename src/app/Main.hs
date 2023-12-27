-- import Debug.Trace (traceM)
import Network.Curl (CurlCode (CurlOK), curlGetString)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)
import Text.Regex (matchRegex, mkRegex)

import PbMetar.Types


main :: IO ()
main = do
  let urlPrefix = "ftp://tgftp.nws.noaa.gov/data/observations/metar/stations" :: String
  let station = "KRDU" :: String
  let url = printf "%s/%s.TXT" urlPrefix station
  result <- resultToEither <$> curlGetString url []
  -- traceM . show $ result
  let parsed = parseResult =<< result
  either exitFail exitOk parsed


resultToEither :: (CurlCode, String) -> Either String String
resultToEither (CurlOK, responseBody) = Right responseBody
resultToEither (curlCode, _) = Left . show $ curlCode


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


-- KRDU 261451Z 09008KT 10SM FEW070 BKN095 BKN110 OVC250 14/08 A3019 RMK AO2 SLP220 T01390083 53003

parseResult :: String -> Either String Metar
parseResult s = do
  let metarString = head . take 1 . drop 1 . lines $ s

  let parsedTime = matchRegex (mkRegex ".* [0-9]{2}([0-9]{2})([0-9]{2})Z .*") metarString
  timeValues <- maybe (Left $ printf "Unable to parse time from: %s" metarString) Right parsedTime
  let time' = Time (read $ timeValues !! 0) (read $ timeValues !! 1)

  let parsedWind = matchRegex (mkRegex ".* [0-9]{3}([0-9]{2})KT .*") metarString
  windValues <- maybe (Left $ printf "Unable to parse wind from: %s" metarString) Right parsedWind
  let wind' = WindKts . read $ windValues !! 0

  let mbTemp = mkTempCelsius $ matchRegex (mkRegex ".* T([0-9]{1})([0-9]{3})[0-9]{4}.*") metarString
  temperature' <- maybe (Left $ printf "Unable to parse temperature from: %s" metarString) Right mbTemp

  pure $ Metar time' wind' temperature'


mkTempCelsius :: Maybe [String] -> Maybe TempCelsius
mkTempCelsius (Just ["0", tempC]) = Just . TempCelsius . (/ 10.0) . read $ tempC
mkTempCelsius (Just ["1", tempC]) = Just . TempCelsius . negate . (/ 10.0) . read $ tempC
mkTempCelsius _ = Nothing
