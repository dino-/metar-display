module PbMetar.Math
  where

import Data.Time.LocalTime (TimeOfDay (..), TimeZone, utcToLocalTimeOfDay)

import PbMetar.Common


calculateWindChill :: (Weather Imperial) -> WindChill Imperial
calculateWindChill (Weather _ (Wind windMph) (Temperature tempF))
  | windMph < 3.0 = NoEffect
  | otherwise = WindChill windChillF
      where
        exp' = 0.16 :: Double
        windChillF = Temperature $ 34.74 + (0.6215 * tempF) - (35.75 * (windMph ** exp')) + (0.4275 * tempF * (windMph ** exp'))

computeLocalTime :: TimeZone -> [String] -> Maybe TimeOfDay
computeLocalTime tz [hs, ms] =
  let (_, localTimeOfDay) = utcToLocalTimeOfDay tz $ TimeOfDay (read hs) (read ms) 0
  in Just localTimeOfDay
computeLocalTime _ _ = Nothing
