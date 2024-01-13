module MetarDisplay.Model.Weather
  where

import Data.Text (pack)
import Data.Time.LocalTime (TimeOfDay (..))
import Text.Mustache (ToMustache, toMustache)

import MetarDisplay.Model.Common (Convert, Imperial, Metric, Station (..), convert)
import MetarDisplay.Model.Temperature
import MetarDisplay.Model.Wind (Gust (..), Wind (..))


data Weather system = Weather
  { station :: Station
  , timeUtc :: TimeOfDay
  , wind :: Wind system
  , gust :: Gust system
  , temperature :: Temperature system
  }
  deriving (Eq, Show)


instance Convert (Weather Imperial) (Weather Metric) where
  convert (Weather station timeUtc wind gust temperature) =
    Weather station timeUtc (convert wind) (convert gust) (convert temperature)

instance Convert (Weather Metric) (Weather Imperial) where
  convert (Weather station timeUtc wind gust temperature) =
    Weather station timeUtc (convert wind) (convert gust) (convert temperature)


data WindChill system
  = WindChill (Temperature system)
  | NoEffect
  deriving (Eq, Show)


instance Convert (WindChill Imperial) (WindChill Metric) where
  convert (WindChill tempF) = WindChill . convert $ tempF
  convert NoEffect = NoEffect

instance ToMustache (WindChill Imperial) where
  toMustache (WindChill tempF) = toMustache tempF
  toMustache NoEffect = toMustache . pack $ "N/A"

instance ToMustache (WindChill Metric) where
  toMustache (WindChill tempC) = toMustache tempC
  toMustache NoEffect = toMustache . pack $ "N/A"


hasChill :: WindChill system -> Bool
hasChill (WindChill _) = True
hasChill NoEffect = False
