module PbMetar.Model.Temperature
  where

import Text.Printf (printf)

import PbMetar.Model.Common


newtype Temperature system = Temperature Double
  deriving (Eq, Show)


instance Convert (Temperature Metric) (Temperature Imperial) where
  convert (Temperature tempC) = Temperature $ tempC * (9 / 5) + 32.0

instance Convert (Temperature Imperial) (Temperature Metric) where
  convert (Temperature tempF) = Temperature $ (tempF - 32.0) * (5 / 9)


instance Format (Temperature Metric) where
  format (Temperature tempC) = printf "%.1f°C" tempC

instance Format (Temperature Imperial) where
  format (Temperature tempF) = printf "%.0f°F" tempF
