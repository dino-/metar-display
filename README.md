# polybar-metar-weather


## Synopsis

Retrieve and parse METAR weather for Polybar


## Description

A utility to retrieve METAR weather info, parse it and construct monitor output
for polybar. Uses FontAwesome glyphs.


## Installation

It's recommended to install a pre-compiled Linux binary either from your
distro's packaging system or from a release on Github but if you like:

    $ cd polybar-metar-weather
    $ stack build
    $ stack install

Which will install it in `~/.local/bin`

You will need a METAR observation station identifier to get weather info. For
the United States, [this chart](https://cnrfc.noaa.gov/metar.php) will be
helpful.

Once you have the program installed, try running it in a terminal

    $ polybar-metar-weather KRDU

You should see output like this

    [2023-12-29 22:41:20 EST : NOTICE] KRDU 300251Z 26005KT 10SM FEW065 04/M02 A2985 RMK AO2 SLP110 T00441017 53017
    %{T2}%{T-} 21:51 %{T2}%{T-} 40°F 4.4°C %{T2}%{T-} 5.8mph %{T2}%{T-} 35°F 1.6°C

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


## Getting source

Source code is available from github at the
[polybar-metar-weather](https://github.com/dino-/polybar-metar-weather) project
page.


## Contact

Dino Morelli <dino@ui3.info>
