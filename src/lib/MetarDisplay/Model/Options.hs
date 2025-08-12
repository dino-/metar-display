module MetarDisplay.Model.Options
  where

import Colog.Simple (Severity (..))
import Data.Text (Text)

import MetarDisplay.Model.Common (Station)


newtype Template = Template Text
  deriving Show


data Verbosity
  = Quiet
  | Verbose Severity
  deriving Show

intToVerbosity :: Int -> Verbosity
intToVerbosity 0 = Quiet
intToVerbosity 1 = Verbose Warning
intToVerbosity 2 = Verbose Notice
intToVerbosity 3 = Verbose Info
intToVerbosity _ = Verbose Debug


newtype LogDate = LogDate Bool
  deriving Show


data Options = Options
  { optVerbosity :: Verbosity
  , optLogDate :: LogDate
  , optStation :: Station
  , optTemplate :: Template
  }
  deriving Show
