module MetarDisplay.Model.Options
  where

import Data.Text (Text)
import System.Log.Logger (Priority (..))

import MetarDisplay.Model.Common (Station)

newtype Template = Template Text
  deriving Show

data Verbosity
  = Quiet
  | Verbose Priority
  deriving Show

intToVerbosity :: Int -> Verbosity
intToVerbosity 0 = Quiet
intToVerbosity 1 = Verbose WARNING
intToVerbosity 2 = Verbose NOTICE
intToVerbosity 3 = Verbose INFO
intToVerbosity _ = Verbose DEBUG

newtype LogDate = LogDate Bool
  deriving Show

data Options = Options
  { optVerbosity :: Verbosity
  , optLogDate :: LogDate
  , optStation :: Station
  , optTemplate :: Template
  }
  deriving Show
