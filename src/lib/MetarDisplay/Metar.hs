{-# LANGUAGE TypeApplications #-}

module MetarDisplay.Metar
  ( isolateMetarLine
  , parse
  )
  where

import Data.Maybe (listToMaybe)
import Data.Monoid (First (..), getFirst)
import Data.Time.LocalTime (TimeOfDay (..))
import Text.Regex (matchRegex, mkRegex)

import MetarDisplay.Model.Common (Meters, Metric, Nautical, Station (..), convert)
import MetarDisplay.Model.Temperature
import MetarDisplay.Model.Weather (Weather (..))
import MetarDisplay.Model.Wind (Gust (..), Wind (..))


isolateMetarLine :: String -> Either String String
isolateMetarLine = Right . head . take 1 . drop 1 . lines


-- KRDU 261451Z 09008KT 10SM FEW070 BKN095 BKN110 OVC250 14/08 A3019 RMK AO2 SLP220 T01390083 53003

parse :: String -> Either String (Weather Metric)
parse metarString = do
  let parsedStation = matchRegex (mkRegex "^([A-Z]{4}) .*") metarString
  station <- (maybe (Left $ "Unable to parse station from: " <> metarString) (Right . Station))
    $ listToMaybe =<< parsedStation

  let parsedTime = matchRegex (mkRegex ".* [0-9]{2}([0-9]{2})([0-9]{2})Z .*") metarString
  time <- (maybe (Left $ "Unable to parse time from: " <> metarString) Right) $ timeFromStrings =<< parsedTime

  let mbWindKt = matchRegex (mkRegex ".* .{3}([0-9]{2,3})(G.*)?KT .*") metarString
  let mbWindMps = matchRegex (mkRegex ".* .{3}([0-9]{2,3})(G.*)?MPS .*") metarString
  (convertWind, windValues) <- maybe (Left $ "Unable to parse wind from: " <> metarString) Right
    $ getFirst . mconcat . map First $ zipWith (\fn wind -> return . (,) fn =<< wind)
        [convert . Wind @Nautical, convert . Wind @Meters, convert . Wind @Nautical]
        [mbWindKt,                 mbWindMps,              Just ["0"]              ]
  let windKph = convertWind . read $ windValues !! 0

  let mbGustKt = matchRegex (mkRegex ".* .{3}[0-9]{2,3}G([0-9]{2,3})KT .*") metarString
  let mbGustMps = matchRegex (mkRegex ".* .{3}[0-9]{2,3}G([0-9]{2,3})MPS .*") metarString
  (convertGust, gustValues) <- maybe (Left $ "Unable to parse gust from: " <> metarString) Right
    $ getFirst . mconcat . map First $ zipWith (\fn gust -> return . (,) fn =<< gust)
        [convert . Gust . Wind @Nautical, convert . Gust . Wind @Meters, const NoGust]
        [mbGustKt,                        mbGustMps,                     Just ["0"]  ]
  let gustKph = convertGust . read $ gustValues !! 0

  let mbTempRmk = mkTempCelsius $ matchRegex (mkRegex ".* T([0-9]{1})([0-9]{3})[0-9]{4}.*") metarString
  let mbTemp = mkTempCelsius $ matchRegex (mkRegex ".* (M)?([0-9]{2,3})/M?[0-9]{2,3} .*") metarString
  tempC <- maybe (Left $ "Unable to parse temperature from: " <> metarString) Right
    $ getFirst . mconcat . map First $ [mbTempRmk, mbTemp]

  let mbDewRmk = mkTempCelsius $ matchRegex (mkRegex ".* T[0-9]{4}([0-9]{1})([0-9]{3}).*") metarString
  let mbDew = mkTempCelsius $ matchRegex (mkRegex ".* M?[0-9]{2,3}/(M)?([0-9]{2,3}) .*") metarString
  dewPointC <- maybe (Left $ "Unable to parse dewpoint from: " <> metarString) Right
    $ getFirst . mconcat . map First $ [mbDewRmk, mbDew]

  pure $ Weather station time windKph gustKph tempC dewPointC


timeFromStrings :: [String] -> Maybe TimeOfDay
timeFromStrings [hs, ms] = Just $ TimeOfDay (read hs) (read ms) 0
timeFromStrings _ = Nothing


mkTempCelsius :: Maybe [String] -> Maybe (Temperature Metric)
mkTempCelsius (Just ["0", tempC]) = Just . Temperature . (/ 10.0) . read $ tempC
mkTempCelsius (Just ["1", tempC]) = Just . Temperature . negate . (/ 10.0) . read $ tempC
mkTempCelsius (Just ["",  tempC]) = Just . Temperature . read $ tempC
mkTempCelsius (Just ["M", tempC]) = Just . Temperature . negate . read $ tempC
mkTempCelsius _ = Nothing
