module PbMetar.Math
  where

import Data.Text (Text, pack)
import Text.Printf (printf)

import PbMetar.Model.Common
import PbMetar.Model.Temperature
import PbMetar.Model.Weather
import PbMetar.Model.Wind


calculateWindChill :: (Weather Imperial) -> WindChill Imperial
calculateWindChill (Weather _ _ (Wind windMph) (Temperature tempF))
  | windMph < 3.0 = NoEffect
  | otherwise = WindChill windChillF
      where
        exp' = 0.16 :: Double
        windChillF = Temperature $ 34.74 + (0.6215 * tempF) - (35.75 * (windMph ** exp')) + (0.4275 * tempF * (windMph ** exp'))


formatTimeValue :: Int -> Text
formatTimeValue val = pack $ printf "%02d" val
