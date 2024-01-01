module PbMetar.Common
  where

import Data.Time.LocalTime (TimeOfDay (..))
import System.Log.Logger (Priority (..))


newtype Station = Station String

newtype FontIndex = FontIndex Int

data Verbosity
  = Quiet
  | Verbose Priority

intToVerbosity :: Int -> Verbosity
intToVerbosity 0 = Quiet
intToVerbosity 1 = Verbose WARNING
intToVerbosity 2 = Verbose NOTICE
intToVerbosity 3 = Verbose INFO
intToVerbosity _ = Verbose DEBUG

data ColorText
  = NoColorChange
  | ColorText String

data Options = Options
  { optVerbosity :: Verbosity
  , optColorText :: ColorText
  , optStation :: Station
  , optFontIndex :: FontIndex
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
