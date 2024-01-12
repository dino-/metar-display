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


data Gust system
  = Gust (Wind system)
  | NoGust
  deriving (Eq, Show)


instance Convert (Gust Imperial) (Gust Metric) where
  convert (Gust windMph) = Gust . convert $ windMph
  convert NoGust = NoGust

instance Convert (Gust Metric) (Gust Imperial) where
  convert (Gust windKph) = Gust . convert $ windKph
  convert NoGust = NoGust

instance Convert (Gust Nautical) (Gust Metric) where
  convert (Gust windKts) = Gust . convert $ windKts
  convert NoGust = NoGust

instance Convert (Gust Meters) (Gust Metric) where
  convert (Gust windMps) = Gust . convert $ windMps
  convert NoGust = NoGust


instance ToMustache (Gust Imperial) where
  toMustache (Gust windMph) = toMustache windMph
  toMustache NoGust = toMustache . pack $ "N/A"

instance ToMustache (Gust Metric) where
  toMustache (Gust windKph) = toMustache windKph
  toMustache NoGust = toMustache . pack $ "N/A"


hasGust :: Gust system -> Bool
hasGust (Gust _) = True
hasGust NoGust = False
