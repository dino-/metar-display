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
mkPolybarLabel (FontIndex fontIndex) colorText weather@(Weather obs _) =
  let (WindMph windMph) = obs.windMph
      (colorBegin, colorEnd) = mkColorFormatting colorText
  in printf "%%{T%d}\xe586%%{T-} %s%02d:%02d%s %%{T%d}\xf2c9%%{T-} %s %%{T%d}\xf72e%%{T-} %s%.1fmph%s"
        fontIndex colorBegin obs.time.todHour obs.time.todMin colorEnd
        fontIndex (mkTempDisplay colorText weather)
        fontIndex colorBegin windMph colorEnd


mkTempDisplay :: ColorText -> Weather -> String
mkTempDisplay colorText (Weather obs chill) =
  let (TempFahr tempF) = obs.tempF
      (TempCelsius tempC) = obs.tempC
      (colorBegin, colorEnd) = mkColorFormatting colorText
      (windChillTempF, windChillTempC) = mkWindChillTemps chill
  in printf "%s%.0f°F%s %.1f°C%s%s" colorBegin tempF windChillTempF tempC windChillTempC colorEnd


mkWindChillTemps :: WindChill -> (String, String)
mkWindChillTemps NoEffect = ("", "")
mkWindChillTemps (WindChill (TempCelsius windChillC) (TempFahr windChillF)) =
  (printf "/%.0f°F" windChillF, printf "/%.1f°C" windChillC)


mkColorFormatting :: ColorText -> (String, String)
mkColorFormatting NoColorChange = ("", "")
mkColorFormatting (ColorText colText) = ( printf "%%{F%s}" colText, "%{F-}")
