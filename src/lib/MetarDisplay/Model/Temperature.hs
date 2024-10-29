module MetarDisplay.Model.Temperature
  where

import Formatting (fixed, format)
import Text.Mustache (ToMustache, toMustache)

import MetarDisplay.Model.Common


newtype Temperature system = Temperature Double
  deriving (Eq, Show)


instance Convert (Temperature Metric) (Temperature Imperial) where
  convert (Temperature tempC) = Temperature $ tempC * (9 / 5) + 32.0

instance Convert (Temperature Imperial) (Temperature Metric) where
  convert (Temperature tempF) = Temperature $ (tempF - 32.0) * (5 / 9)


instance ToMustache (Temperature Metric) where
  toMustache (Temperature tempC) = toMustache $ format (fixed 1) tempC

instance ToMustache (Temperature Imperial) where
  toMustache (Temperature tempF) = toMustache $ format (fixed 0) tempF
