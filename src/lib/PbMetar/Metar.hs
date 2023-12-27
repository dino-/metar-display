module PbMetar.Metar
  ( parse
  )
  where

import Text.Printf (printf)
import Text.Regex (matchRegex, mkRegex)

import PbMetar.Types


-- KRDU 261451Z 09008KT 10SM FEW070 BKN095 BKN110 OVC250 14/08 A3019 RMK AO2 SLP220 T01390083 53003

parse :: String -> Either String Metar
parse s = do
  let metarString = head . take 1 . drop 1 . lines $ s

  let parsedTime = matchRegex (mkRegex ".* [0-9]{2}([0-9]{2})([0-9]{2})Z .*") metarString
  timeValues <- maybe (Left $ printf "Unable to parse time from: %s" metarString) Right parsedTime
  let time' = Time (read $ timeValues !! 0) (read $ timeValues !! 1)

  let parsedWind = matchRegex (mkRegex ".* [0-9]{3}([0-9]{2,3})KT .*") metarString
  windValues <- maybe (Left $ printf "Unable to parse wind from: %s" metarString) Right parsedWind
  let wind' = WindKts . read $ windValues !! 0

  let mbTemp = mkTempCelsius $ matchRegex (mkRegex ".* T([0-9]{1})([0-9]{3})[0-9]{4}.*") metarString
  temperature' <- maybe (Left $ printf "Unable to parse temperature from: %s" metarString) Right mbTemp

  pure $ Metar time' wind' temperature'


mkTempCelsius :: Maybe [String] -> Maybe TempCelsius
mkTempCelsius (Just ["0", tempC]) = Just . TempCelsius . (/ 10.0) . read $ tempC
mkTempCelsius (Just ["1", tempC]) = Just . TempCelsius . negate . (/ 10.0) . read $ tempC
mkTempCelsius _ = Nothing
