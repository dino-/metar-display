module PbMetar.Model.Temperature
  where

import PbMetar.Model.Common


newtype Temperature system = Temperature Double
  deriving (Eq, Show)

instance Convert (Temperature Metric) (Temperature Imperial) where
  convert (Temperature tempC) = Temperature $ tempC * (9 / 5) + 32.0

instance Convert (Temperature Imperial) (Temperature Metric) where
  convert (Temperature tempF) = Temperature $ (tempF - 32.0) * (5 / 9)
