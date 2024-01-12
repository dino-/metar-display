module PbMetar.Model.Wind
  where

import Data.Text (pack)
import Text.Mustache (ToMustache, toMustache)
import Text.Printf (printf)

import PbMetar.Model.Common (Convert, Imperial, Meters, Metric, Nautical, convert)


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


instance ToMustache (Wind Metric) where
  toMustache (Wind windKph) = toMustache . pack $ printf "%.0f" windKph

instance ToMustache (Wind Imperial) where
  toMustache (Wind windMph) = toMustache . pack $ printf "%.1f" windMph
