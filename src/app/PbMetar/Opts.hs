{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module PbMetar.Opts
   ( parseOpts
   )
   where

import Data.Char (toUpper)
import Data.Version (showVersion)
import Options.Applicative
import Paths_polybar_metar_weather (version)
import Prettyprinter (pretty)
import System.Environment (getProgName)
import Text.Heredoc (here)
import Text.Printf (printf)

import PbMetar.Common (FontIndex (..), Options (..), Station (..), intToVerbosity)


parser :: Parser Options
parser = Options
  <$> ( intToVerbosity <$> option auto
        (  long "verbose"
        <> short 'v'
        <> metavar "NUM"
        <> help "Verbosity level. 0=quiet, 1=warnings/errors, 2=normal messages, 3=more info, 4=debug"
        <> showDefault
        <> value 2
        )
      )
  <*> ( Station . map toUpper <$> strArgument
        (  metavar "STATION"
        <> help "Retrieve weather data for this station. See STATION below"
        )
      )
  <*> ( FontIndex <$> argument auto
        (  metavar "FONT-INDEX"
        <> help "Index of polybar font for Font Awesome glyphs"
        <> showDefault
        <> value 2
        )
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

The default FONT-INDEX assumes it's the second one declared in polybar's
config.ini (font-1)

But note FONT-INDEX is often not necessary even if the index is wrong because polybar
tries to figure out a loaded font that has the glyphs needed. You'll probably
need it only when using more than one glyph font.

Version %s  Dino Morelli <dino@ui3.info>|]
