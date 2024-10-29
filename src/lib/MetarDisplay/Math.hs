module MetarDisplay.Math
  where

import Data.Text (Text)
import Formatting (left, sformat)

import MetarDisplay.Model.Common (Imperial)
import MetarDisplay.Model.Humidity
import MetarDisplay.Model.Temperature
import MetarDisplay.Model.Weather (Weather (..), WindChill (..))
import MetarDisplay.Model.Wind (Wind (..))


calculateWindChill :: (Weather Imperial) -> WindChill Imperial
calculateWindChill (Weather _ _ (Wind windMph) _ (Temperature tempF) _)
  | (windMph < 3.0) || (chillF >= tempF) = NoEffect
  | otherwise = WindChill $ Temperature chillF
  where
    exp' = 0.16 :: Double
    chillF = 34.74 + (0.6215 * tempF)
      - (35.75 * (windMph ** exp'))
      + (0.4275 * tempF * (windMph ** exp'))


formatTimeValue :: Int -> Text
formatTimeValue val = sformat (left 2 '0') val


calculateRelativeHumidity :: Temperature system -> Temperature system -> RelativeHumidity
calculateRelativeHumidity (Temperature tempC) (Temperature dewPointC) =
  RelativeHumidity $ negate $ (5.0 * tempC) - (5.0 * dewPointC) - 100
