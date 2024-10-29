module MetarDisplay.Model.Humidity
  where

import Formatting (fixed, format)
import Text.Mustache (ToMustache, toMustache)


newtype RelativeHumidity = RelativeHumidity Double
  deriving (Eq, Show)

instance ToMustache RelativeHumidity where
  toMustache (RelativeHumidity rh) = toMustache $ format (fixed 0) rh
