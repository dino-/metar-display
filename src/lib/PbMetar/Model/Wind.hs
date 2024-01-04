module PbMetar.Model.Wind
  where

import Text.Printf (printf)

import PbMetar.Model.Common


newtype Wind system = Wind Double
  deriving (Eq, Show)


instance Convert (Wind Metric) (Wind Imperial) where
  convert (Wind windKph) = Wind $ windKph * 0.62137119

instance Convert (Wind Imperial) (Wind Metric) where
  convert (Wind windMph) = Wind $ windMph * 1.609344

instance Convert (Wind Nautical) (Wind Metric) where
  convert (Wind windKts) = Wind $ windKts * 1.852

instance Convert (Wind Meters) (Wind Metric) where
  convert (Wind windMps) = Wind $ windMps * 3.6


instance Format (Wind Imperial) where
  format (Wind windMph) = printf "%.1fmph" windMph

instance Format (Wind Metric) where
  format (Wind windKph) = printf "%.0fkph" windKph
