{-# LANGUAGE QuasiQuotes #-}

module MetarDisplay.Opts
   ( parseOpts
   )
   where

import Data.Char (toUpper)
import Data.Version (showVersion)
import Formatting ((%+), format, formatToString)
import Formatting.ShortFormatters (s)
import Options.Applicative
import Paths_metar_display (version)
import Prettyprinter (pretty)
import System.Environment (getProgName)
import Text.Heredoc (here)

import MetarDisplay.Model.Common (Station (..))
import MetarDisplay.Model.Options (LogDate (..), Options (..), Template (..), intToVerbosity)


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
  <*> ( LogDate . not <$> switch
        (  long "no-date"
        <> short 'D'
        <> help "Do not add date/time to log messages, useful when logging to systemd"
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
  infoOption (formatToString (s %+ s) progName (showVersion version)) $ mconcat
  [ long "version"
  , help "Show version information"
  , hidden
  ]


parseOpts :: IO Options
parseOpts = do
  pn <- getProgName
  execParser $ info (parser <**> helper <**> versionHelper pn)
    (  header (formatToString (s %+ "- Retrieve METAR weather, decode it and build customizable output of the data") pn)
    <> footer'
    )


footer' :: InfoMod a
footer' = footerDoc . Just . pretty . format content . showVersion $ version
  where content = [here|STATION

You will need a METAR observation station identifier to get weather info. For the United States this chart will be helpful: https://cnrfc.noaa.gov/metar.php

Once you have a station, try running it in a terminal

    $ metar-display KRDU

You should see output like this

    [2024-01-11 17:00:13 EST : NOTICE] KRDU 112151Z 23006KT 10SM FEW250 13/00 A3000 RMK AO2 SLP160 T01280000
    KRDU: 12.8°C (04)

TEMPLATE

The template is in Mustache format with these weather value fields available:

    field     description                                       example
    -------------------------------------------------------------------
    station     Station code                                    KRDU
    hour12      Hour of observation 12-hour format              03
    hour24      Hour of observation 24-hour format              15
    min         Minute of observation                           47
    windKph     Wind speed in KPH                               11
    windMph     Wind speed in MPH                               6.9
    hasGust     True if there are wind gusts (for use with {{#hasGust}}...{{/hasGust}} conditionals)
    gustKph     Wind gust speed in KPH                          38
    gustMph     Wind gust speed in MPH                          41.2
    tempC       Temperature in degrees Celsius                  11.3
    tempF       Temperature in degrees Fahrenheit               56
    hasChill    True if there is wind chill effect (for use with {{#hasChill}}...{{/hasChill}} conditionals)
    chillC      Wind chill temperature in degrees Celsius       2.7
    chillF      Wind chill temperature in degrees Fahrenheit    30
    dewPointC   Dewpoint temperature in degrees Celsius         -2.7
    dewPointF   Dewpoint temperature in degrees Fahrenheit      28
    rh          Relative humidity as a perentage                65

Default template

    {{station}}: {{tempC}}°C ({{hour12}})

A template with conditional wind chill and wind gusts

    {{station}} ({{hour24}}): {{tempF}}°F{{#hasChill}} ({{chillF}}°F {{windMph}}mph){{/hasChill}}{{#hasGust}} gust: {{gustMph}}mph{{/hasGust}}

The same template with Font Awesome icons and colored text (you might use this with polybar, see below)

     %{F#f0c674}{{station}} ({{hour24}})%{F-}  %{F#f0c674}{{tempF}}°F{{#hasChill}} ({{chillF}}°F {{windMph}}mph){{/hasChill}}%{F-}{{#hasGust}}  %{F#f0c674}{{gustMph}}mph%{F-}{{/hasGust}}

INTEGRATION WITH POLYBAR

The output of this program can be used with polybar, here's how to configure that. In your polybar config.ini

    [module/weather]
    type = custom/script

    ; When configuring the display template, you may not need font switching notation (%{T2}...%{T-}) if there's only one symbol font, polybar can often figure it out.

    command = "path/if/not/on/PATH/metar-display KRDU ' %{F#f0c674}{{station}} ({{hour24}})%{F-}  %{F#f0c674}{{tempF}}°F{{#hasChill}} ({{chillF}}°F {{windMph}}mph){{/hasChill}}%{F-}{{#hasGust}}  %{F#f0c674}{{gustMph}}mph%{F-}{{/hasGust}}' 2>> ~/.xmonad/metar-display.log"

    exec = ${self.command}
    click-left = ${self.command}

    ; 3600 secs = 60 minutes, METAR records are often updated hourly
    interval = 3600

Note the `click-left` definition, which will re-run the module immediately.

And then include the weather module in one of your bar sections

    modules-right = ... weather ...

Here's an example of bringing Font Awesome into your polybar, if you don't already have it (in the [bar/YOURBARNAME] section):

    font-1 = Font Awesome 6 Free,Font Awesome 6 Free Solid:style=Solid:size=16;4


Version|] %+ s %+ " Dino Morelli <dino@ui3.info>"
