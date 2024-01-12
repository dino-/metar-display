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

import PbMetar.Model.Common (Station (..))
import PbMetar.Model.Options (Options (..), Template (..), intToVerbosity)


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
  <*> ( Template <$> strArgument
        (  metavar "TEMPLATE"
        <> help "Mustache template for output formatting. See TEMPLATE below"
        <> showDefault
        <> value "{{station}}: {{tempC}}°C ({{hour12}})"
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
    (  header (printf "%s - Retrieve METAR weather, decode it and build customizable output of the data" pn)
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

    [2024-01-11 17:00:13 EST : NOTICE] KRDU 112151Z 23006KT 10SM FEW250 13/00 A3000 RMK AO2 SLP160 T01280000
    KRDU: 12.8°C (04)

TEMPLATE

The template is a standard Mustache template where weather values will be substituted for these field names:

    field     description                                     example
    -------------------------------------------------------------------
    station   Station code                                    KRDU
    hour12    Hour of observation 12-hour format              03
    hour24    Hour of observation 24-hour format              15
    min       Minute of observation                           47
    windKph   Wind speed in KPH                               11
    windMph   Wind speed in MPH                               6.9
    hasGust   True if there are wind gusts (for use with {{#hasGust}}...{{/hasGust}} conditionals)
    gustKph   Wind gust speed in KPH                          38
    gustMph   Wind gust speed in MPH                          41.2
    tempC     Temperature in degrees Celsius                  11.3
    tempF     Temperature in degrees Fahrenheit               56
    hasChill  True if there is wind chill effect (for use with {{#hasChill}}...{{/hasChill}} conditionals)
    chillC    Wind chill temperature in degrees Celsius       2.7
    chillF    Wind chill temperature in degrees Fahrenheit    30

INTEGRATION WITH POLYBAR

The output of this program can be used with polybar, here's how to configure
that. In your polybar config.ini

    [module/weather]
    type = custom/script

    command = "path/if/not/on/PATH/polybar-metar-weather KXYZ 2>> ~/.xmonad/polybar-metar-weather.log"

    ; Specifying your own template string
    ; command = "path/if/not/on/PATH/polybar-metar-weather KXYZ '{{station}}: {{tempF}}°F ({{hour24}})' 2>> ~/.xmonad/polybar-metar-weather.log"
    ; A fancier template with Font Awesome glyphs, colored text and conditional wind chill and wind gust display!
    ; command = "path/if/not/on/PATH/polybar-metar-weather KXYZ '%%{T2}%%{T-} %%{F#f0c674}{{station}} {{hour24}}:{{min}}%%{F-} %%{T2}%%{T-} %%{F#f0c674}{{tempF}}°F{{#hasChill}}/{{chillF}}°F{{/hasChill}}%%{F-}{{#hasGust}} %%{T2}%%{T-} %%{F#f0c674}{{gustMph}}mph%%{F-}{{/hasGust}}' 2>> ~/.xmonad/polybar-metar-weather.log"

    exec = ${self.command}
    click-left = ${self.command}

    ; 3600 secs = 60 minutes, METAR records are often updated hourly
    interval = 3600

Note the `click-left` definition, which will re-run the module immediately.

Here's an example of bringing Font Awesome into your polybar, if you don't
already have it (in the [bar/YOURBARNAME] section):

    font-1 = Font Awesome 6 Free,Font Awesome 6 Free Solid:style=Solid:size=16;4

And then include the weather module in one of your bar sections

    modules-right = ... weather ...


Version %s  Dino Morelli <dino@ui3.info>|]
