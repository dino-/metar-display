module PbMetar.Model.Options
  where

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
