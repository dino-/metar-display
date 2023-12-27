module PbMetar.Common
  where

import Data.Time.LocalTime (TimeOfDay (..))


newtype WindKts = WindKts Double
  deriving Show

newtype WindMph = WindMph Double
  deriving Show

newtype TempCelsius = TempCelsius Double
  deriving Show

newtype TempFahr = TempFahr Double

data Metar = Metar
  { time :: TimeOfDay
  , wind :: WindKts
  , temperature :: TempCelsius
  }
  deriving Show
