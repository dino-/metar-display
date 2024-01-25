module MetarDisplay.Math
  where

import Data.Text (Text, pack)
import Text.Printf (printf)

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
formatTimeValue val = pack $ printf "%02d" val


calculateRelativeHumidity :: Temperature system -> Temperature system -> RelativeHumidity
calculateRelativeHumidity (Temperature tempC) (Temperature dewPointC) =
  RelativeHumidity $ negate $ (5.0 * tempC) - (5.0 * dewPointC) - 100
