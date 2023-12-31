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
  deriving (Eq, Show)

newtype WindMph = WindMph Double
  deriving (Eq, Show)

newtype TempCelsius = TempCelsius Double
  deriving (Eq, Show)

newtype TempFahr = TempFahr Double
  deriving (Eq, Show)

data Observations = Observations
  { time :: TimeOfDay
  , windKts :: WindKts
  , tempC :: TempCelsius
  , windMph :: WindMph
  , tempF :: TempFahr
  }
  deriving (Eq, Show)

data WindChill
  = WindChill TempCelsius TempFahr
  | NoEffect
  deriving (Eq, Show)

data Weather = Weather Observations WindChill
  deriving (Eq, Show)
