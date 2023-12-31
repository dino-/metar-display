module PbMetar.Common
  where

import Data.Time.LocalTime (TimeOfDay (..))


newtype Station = Station String

-- newtype FontIndex = FontIndex Int

data Options = Options
  { optStation :: Station
  -- , optStation :: FontIndex
  }


newtype WindKts = WindKts Double
  deriving Show

newtype WindMph = WindMph Double
  deriving Show

newtype TempCelsius = TempCelsius Double
  deriving Show

newtype TempFahr = TempFahr Double

data Observations = Observations
  { time :: TimeOfDay
  , windKts :: WindKts
  , tempC :: TempCelsius
  , windMph :: WindMph
  , tempF :: TempFahr
  }

data WindChill
  = WindChill TempCelsius TempFahr
  | NoEffect

data Weather = Weather Observations WindChill
