{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module PbMetar.Opts
   ( parseOpts
   )
   where

import Data.Version (showVersion)
import Options.Applicative
import Paths_polybar_metar_weather (version)
import Prettyprinter (pretty)
import System.Environment (getProgName)
-- import System.Log (Priority (INFO))
import Text.Heredoc (here)
import Text.Printf (printf)

import PbMetar.Common (Options (..), Station (..))


parser :: Parser Options
parser = Options
  <$> Station <$> strArgument
        (  metavar "STATION"
        <> help "Retrieve weather data for this station. See STATION below"
        )


versionHelper :: String -> Parser (a -> a)
versionHelper progName =
  infoOption (printf "%s %s" progName (showVersion version)) $ mconcat
  [ long "version"
  , help "Show version information"
  , hidden
  ]


parseOpts :: IO Options
parseOpts = do
  pn <- getProgName
  execParser $ info (parser <**> helper <**> versionHelper pn)
    (  header (printf "%s - Retrieve METAR weather and build a polybar format string" pn)
    <> footer'
    )


footer' :: InfoMod a
footer' = footerDoc . Just . pretty $ (printf content (showVersion version) :: String)
  where content = [here|OVERVIEW

Overview text goes here.

Version %s  Dino Morelli <dino@ui3.info>|]
