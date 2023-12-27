module PbMetar.Types
  where


-- Time values from METAR are UTC
data Time = Time Int Int
  deriving Show

newtype WindKts = WindKts Double
  deriving Show

newtype WindMph = WindMph Double
  deriving Show

newtype TempCelsius = TempCelsius Double
  deriving Show

newtype TempFahr = TempFahr Double

data Metar = Metar
  { time :: Time
  , wind :: WindKts
  , temperature :: TempCelsius
  }
  deriving Show
