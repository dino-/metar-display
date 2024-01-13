module MetarDisplay.Math
  where

import Data.Text (Text, pack)
import Text.Printf (printf)

import MetarDisplay.Model.Common (Imperial)
import MetarDisplay.Model.Temperature
import MetarDisplay.Model.Weather (Weather (..), WindChill (..))
import MetarDisplay.Model.Wind (Wind (..))


calculateWindChill :: (Weather Imperial) -> WindChill Imperial
calculateWindChill (Weather _ _ (Wind windMph) _ (Temperature tempF))
  | windMph < 3.0 = NoEffect
  | otherwise = WindChill windChillF
      where
        exp' = 0.16 :: Double
        windChillF = Temperature $ 34.74 + (0.6215 * tempF) - (35.75 * (windMph ** exp')) + (0.4275 * tempF * (windMph ** exp'))


formatTimeValue :: Int -> Text
formatTimeValue val = pack $ printf "%02d" val
