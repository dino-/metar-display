module PbMetar.Math
  where

import Data.Time.LocalTime (TimeOfDay (..), TimeZone, utcToLocalTimeOfDay)

import PbMetar.Common


celsiusToFahrenheit :: TempCelsius -> TempFahr
celsiusToFahrenheit (TempCelsius tempCelsius) = TempFahr $ tempCelsius * (9 / 5) + 32.0


fahrenheitToCelsius :: TempFahr -> TempCelsius
fahrenheitToCelsius (TempFahr tempFahr) = TempCelsius $ (tempFahr - 32) * (5 / 9)


knotsToMph :: WindKts -> WindMph
knotsToMph (WindKts knots) = WindMph $ knots * 6076 / 5280


mpsToKnots :: Double -> WindKts
mpsToKnots mps = WindKts $ mps * 1.9438445


calculateWindChill :: WindMph -> TempFahr -> WindChill
calculateWindChill (WindMph wind) (TempFahr tempF)
  | wind < 3.0 = NoEffect
  | otherwise = WindChill tempC' tempF'
      where
        exp' = 0.16 :: Double
        tempF' = TempFahr $ 34.74 + (0.6215 * tempF) - (35.75 * (wind ** exp')) + (0.4275 * tempF * (wind ** exp'))
        tempC' = fahrenheitToCelsius tempF'


computeLocalTime :: TimeZone -> [String] -> Maybe TimeOfDay
computeLocalTime tz [hs, ms] =
  let (_, localTimeOfDay) = utcToLocalTimeOfDay tz $ TimeOfDay (read hs) (read ms) 0
  in Just localTimeOfDay
computeLocalTime _ _ = Nothing
