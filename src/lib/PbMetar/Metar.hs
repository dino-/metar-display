{-# LANGUAGE TypeApplications #-}

module PbMetar.Metar
  ( isolateMetarLine
  , parse
  )
  where

import Data.Monoid (First (..), getFirst)
import Data.Time.LocalTime (TimeOfDay (..))
import Text.Regex (matchRegex, mkRegex)

import PbMetar.Model.Common
import PbMetar.Model.Temperature
import PbMetar.Model.Weather
import PbMetar.Model.Wind


isolateMetarLine :: String -> Either String String
isolateMetarLine = Right . head . take 1 . drop 1 . lines


-- KRDU 261451Z 09008KT 10SM FEW070 BKN095 BKN110 OVC250 14/08 A3019 RMK AO2 SLP220 T01390083 53003

parse :: String -> Either String (Weather Metric)
parse metarString = do
  let parsedTime = matchRegex (mkRegex ".* [0-9]{2}([0-9]{2})([0-9]{2})Z .*") metarString
  time <- (maybe (Left $ "Unable to parse time from: " <> metarString) Right) $ timeFromStrings =<< parsedTime

  let mbWindKt = matchRegex (mkRegex ".* .{3}([0-9]{2,3})(G.*)?KT .*") metarString
  let mbWindMps = matchRegex (mkRegex ".* .{3}([0-9]{2,3})(G.*)?MPS .*") metarString
  (conversionFunction, windValues) <- maybe (Left $ "Unable to parse wind from: " <> metarString) Right
    $ getFirst . mconcat . map First $ zipWith (\fn wind -> return . (,) fn =<< wind)
        [convert . Wind @Nautical, convert . Wind @Meters, convert . Wind @Nautical]
        [mbWindKt,                 mbWindMps,              Just ["0"]              ]
  let windKph = conversionFunction . read $ windValues !! 0

  let mbTempRmk = mkTempCelsius $ matchRegex (mkRegex ".* T([0-9]{1})([0-9]{3})[0-9]{4}.*") metarString
  let mbTemp = mkTempCelsius $ matchRegex (mkRegex ".* (M)?([0-9]{2,3})/M?[0-9]{2,3} .*") metarString
  tempC <- maybe (Left $ "Unable to parse temperature from: " <> metarString) Right
    $ getFirst . mconcat . map First $ [mbTempRmk, mbTemp]

  pure $ Weather time windKph tempC


timeFromStrings :: [String] -> Maybe TimeOfDay
timeFromStrings [hs, ms] = Just $ TimeOfDay (read hs) (read ms) 0
timeFromStrings _ = Nothing


mkTempCelsius :: Maybe [String] -> Maybe (Temperature Metric)
mkTempCelsius (Just ["0", tempC]) = Just . Temperature . (/ 10.0) . read $ tempC
mkTempCelsius (Just ["1", tempC]) = Just . Temperature . negate . (/ 10.0) . read $ tempC
mkTempCelsius (Just ["",  tempC]) = Just . Temperature . read $ tempC
mkTempCelsius (Just ["M", tempC]) = Just . Temperature . negate . read $ tempC
mkTempCelsius _ = Nothing
