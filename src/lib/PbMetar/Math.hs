module PbMetar.Math
  where

import PbMetar.Types


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
