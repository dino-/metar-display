module PbMetar.Model.Weather
  where

import Data.Time.LocalTime (TimeOfDay (..))

import PbMetar.Model.Common
import PbMetar.Model.Temperature
import PbMetar.Model.Wind


data Weather system = Weather
  { timeUtc :: TimeOfDay
  , wind :: Wind system
  , temperature :: Temperature system
  }
  deriving (Eq, Show)


instance Convert (Weather Imperial) (Weather Metric) where
  convert (Weather timeUtc wind temperature) = Weather timeUtc (convert wind) (convert temperature)

instance Convert (Weather Metric) (Weather Imperial) where
  convert (Weather timeUtc wind temperature) = Weather timeUtc (convert wind) (convert temperature)


data WindChill system
  = WindChill (Temperature system)
  | NoEffect
  deriving (Eq, Show)


instance Format (WindChill Imperial) where
  format NoEffect = ""
  format (WindChill tempF) = format tempF

instance Format (WindChill Metric) where
  format NoEffect = ""
  format (WindChill tempC) = format tempC
