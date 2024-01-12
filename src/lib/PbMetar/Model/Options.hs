module PbMetar.Model.Options
  where

import Data.Text (Text)
import System.Log.Logger (Priority (..))

import PbMetar.Model.Common (Station)

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

data Options = Options
  { optVerbosity :: Verbosity
  , optStation :: Station
  , optTemplate :: Template
  }
  deriving Show
