module PbMetar.Math
  where

import PbMetar.Common


calculateWindChill :: (Weather Imperial) -> WindChill Imperial
calculateWindChill (Weather _ (Wind windMph) (Temperature tempF))
  | windMph < 3.0 = NoEffect
  | otherwise = WindChill windChillF
      where
        exp' = 0.16 :: Double
        windChillF = Temperature $ 34.74 + (0.6215 * tempF) - (35.75 * (windMph ** exp')) + (0.4275 * tempF * (windMph ** exp'))
