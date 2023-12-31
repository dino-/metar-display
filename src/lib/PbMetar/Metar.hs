module PbMetar.Metar
  ( isolateMetarLine
  , parse
  )
  where

import Data.Time.LocalTime (TimeZone)
import Text.Printf (printf)
import Text.Regex (matchRegex, mkRegex)

import PbMetar.Common
import PbMetar.Math (calculateWindChill, celsiusToFahrenheit, computeLocalTime, knotsToMph)


isolateMetarLine :: String -> Either String String
isolateMetarLine = Right . head . take 1 . drop 1 . lines


-- KRDU 261451Z 09008KT 10SM FEW070 BKN095 BKN110 OVC250 14/08 A3019 RMK AO2 SLP220 T01390083 53003

parse :: TimeZone -> String -> Either String Weather
parse tz metarString = do
  let parsedTime = matchRegex (mkRegex ".* [0-9]{2}([0-9]{2})([0-9]{2})Z .*") metarString
  time' <- maybe (Left $ printf "Unable to parse time from: %s" metarString) Right $ computeLocalTime tz =<< parsedTime

  let parsedWind = matchRegex (mkRegex ".* .{3}([0-9]{2,3})(G.*)?KT .*") metarString
  windValues <- maybe (Left $ printf "Unable to parse wind from: %s" metarString) Right parsedWind
  let windKts' = WindKts . read $ windValues !! 0

  let mbTemp = mkTempCelsius $ matchRegex (mkRegex ".* T([0-9]{1})([0-9]{3})[0-9]{4}.*") metarString
  tempC' <- maybe (Left $ printf "Unable to parse temperature from: %s" metarString) Right mbTemp

  let tempF' = celsiusToFahrenheit tempC'
  let obs = Observations time' windKts' tempC' (knotsToMph windKts') tempF'
  let chill = calculateWindChill (knotsToMph windKts') tempF'
  pure $ Weather obs chill


mkTempCelsius :: Maybe [String] -> Maybe TempCelsius
mkTempCelsius (Just ["0", tempC]) = Just . TempCelsius . (/ 10.0) . read $ tempC
mkTempCelsius (Just ["1", tempC]) = Just . TempCelsius . negate . (/ 10.0) . read $ tempC
mkTempCelsius _ = Nothing
