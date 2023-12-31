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
        <> help "Index of polybar font for Font Awesome glyphs. See FONT-INDEX below"
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
  where content = [here|STATION

You will need a METAR observation station identifier to get weather info. For
the United States this chart will be helpful: https://cnrfc.noaa.gov/metar.php

Once you have a station, try running it in a terminal

    $ polybar-metar-weather KRDU

You should see output like this

    [2023-12-29 22:41:20 EST : NOTICE] KRDU 300251Z 26005KT 10SM FEW065 04/M02 A2985 RMK AO2 SLP110 T00441017 53017
    %%{T2}%%{T-} 21:51 %%{T2}%%{T-} 40°F 4.4°C %%{T2}%%{T-} 5.8mph %%{T2}%%{T-} 35°F 1.6°C

INTEGRATION WITH POLYBAR

polybar-metar-weather must then be integrated with polybar in your polybar
config.

    [module/weather]
    type = custom/script

    command = "path/if/not/on/PATH/polybar-metar-weather KXYZ 2>> ~/.xmonad/polybar-metar-weather.log"
    exec = ${self.command}

    click-left = ${self.command}

    ; 3600 secs = 60 minutes, METAR records are often updated hourly
    interval = 3600

Note the `click-left` definition, which will re-run the module immediately.

Also note the `2>> ~/.xmonad/...` part of the command. This will append
polybar-metar-weather's stderr log output to the named file.

For the icons this module is expecting Font Awesome to be installed and
configured as the 2nd font (`font-1`), if you don't already have this.

    font-1 = Font Awesome 6 Free,Font Awesome 6 Free Solid:style=Solid:size=16;4

And then include the weather module in one of your bar sections

    modules-right = ... weather ...

FONT-INDEX

The default FONT-INDEX assumes it's the second one declared in polybar's
config.ini (font-1)

But note FONT-INDEX is often not necessary even if the index is wrong because polybar
tries to figure out a loaded font that has the glyphs needed. You'll probably
need it only when using more than one glyph font.


Version %s  Dino Morelli <dino@ui3.info>|]
