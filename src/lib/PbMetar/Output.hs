{-# LANGUAGE OverloadedRecordDot #-}

module PbMetar.Output
  ( mkPolybarLabel
  )
  where

import Data.Time.LocalTime (TimeOfDay (..))
import Text.Printf (printf)

import PbMetar.Common
  ( ColorText (NoColorChange, ColorText)
  , FontIndex (..)
  , Observations (..)
  , TempCelsius (..)
  , TempFahr (..)
  , Weather (..)
  , WindChill (NoEffect, WindChill)
  , WindMph (..)
  )


mkPolybarLabel :: FontIndex -> ColorText -> Weather -> String
mkPolybarLabel fi@(FontIndex fontIndex) colorText (Weather obs chill) = do
  let (TempFahr tempF) = obs.tempF
  let (TempCelsius tempC) = obs.tempC
  let (WindMph windMph) = obs.windMph
  let (colorBegin, colorEnd) = mkColorFormatting colorText
  printf "%%{T%d}\xe586%%{T-} %s%02d:%02d%s %%{T%d}\xf2c9%%{T-} %s%.0f°F %01f°C%s %%{T%d}\xf72e%%{T-} %s%.1fmph%s%s"
        fontIndex colorBegin obs.time.todHour obs.time.todMin colorEnd
        fontIndex colorBegin tempF tempC colorEnd
        fontIndex colorBegin windMph colorEnd (mkWindChillDisplay fi colorText chill)


mkWindChillDisplay :: FontIndex -> ColorText -> WindChill -> String
mkWindChillDisplay (FontIndex fontIndex) colorText (WindChill (TempCelsius windChillC) (TempFahr windChillF)) =
  printf " %%{T%d}\xf7ad%%{T-} %s%.0f°F %.1f°C%s" fontIndex colorBegin windChillF windChillC colorEnd
  where (colorBegin, colorEnd) = mkColorFormatting colorText
mkWindChillDisplay _ _ NoEffect = ""


mkColorFormatting :: ColorText -> (String, String)
mkColorFormatting NoColorChange = ("", "")
mkColorFormatting (ColorText colText) = ( printf "%%{F%s}" colText, "%{F-}")
