module MetarDisplay.Model.Humidity
  where

import Data.Text (pack)
import Text.Mustache (ToMustache, toMustache)
import Text.Printf (printf)

-- import MetarDisplay.Model.Common (Convert, Imperial, Meters, Metric, Nautical, convert)


newtype RelativeHumidity = RelativeHumidity Double
  deriving (Eq, Show)

instance ToMustache RelativeHumidity where
  toMustache (RelativeHumidity rh) = toMustache . pack $ printf "%.0f" rh
