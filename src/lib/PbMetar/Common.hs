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


data Imperial   -- Units like degrees Fahrenheit, miles per hour, for human-readability
data Metric     -- Units like degrees Celsius, kilometers per hour, for human-readability
data Nautical   -- For wind speed in knots, METAR wind data almost always comes in this form
data Meters     -- The odd case where units come in from METAR in meters per second

class Convert systemFrom systemTo where
  convert :: systemFrom -> systemTo


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


newtype Temperature system = Temperature Double
  deriving (Eq, Show)

instance Convert (Temperature Metric) (Temperature Imperial) where
  convert (Temperature tempC) = Temperature $ tempC * (9 / 5) + 32.0

instance Convert (Temperature Imperial) (Temperature Metric) where
  convert (Temperature tempF) = Temperature $ (tempF - 32.0) * (5 / 9)


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
