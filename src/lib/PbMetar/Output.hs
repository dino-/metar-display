module PbMetar.Output
  ( mkPolybarLabel
  )
  where

import Data.Time.LocalTime (TimeOfDay (..), TimeZone, utcToLocalTimeOfDay)
import Text.Printf (printf)

import PbMetar.Model.Common
import PbMetar.Model.Options
import PbMetar.Model.Temperature
import PbMetar.Model.Weather
import PbMetar.Model.Wind
import PbMetar.Math (calculateWindChill)


mkPolybarLabel :: TimeZone -> FontIndex -> ColorText -> Weather Metric -> String
mkPolybarLabel localZone (FontIndex fontIndex) colorText weatherM =
  let weatherI = convert weatherM :: Weather Imperial
      (_, (TimeOfDay localHour localMin _)) = utcToLocalTimeOfDay localZone $ timeUtc weatherI
      (Wind windMph) = wind weatherI
      (colorBegin, colorEnd) = mkColorFormatting colorText
  in printf "%%{T%d}\xe586%%{T-} %s%02d:%02d%s %%{T%d}\xf2c9%%{T-} %s %%{T%d}\xf72e%%{T-} %s%.1fmph%s"
        fontIndex colorBegin localHour localMin colorEnd
        fontIndex (mkTempDisplay colorText weatherI)
        fontIndex colorBegin windMph colorEnd


mkTempDisplay :: ColorText -> Weather Imperial -> String
mkTempDisplay colorText weatherI =
  let (Temperature tempF) = temperature weatherI
      (colorBegin, colorEnd) = mkColorFormatting colorText
      windChillLabel = mkWindChillLabel $ calculateWindChill weatherI
  in printf "%s%.0f°F%s%s" colorBegin tempF windChillLabel colorEnd


mkWindChillLabel :: WindChill Imperial -> String
mkWindChillLabel NoEffect = ""
mkWindChillLabel (WindChill (Temperature windChillF)) =
  printf "/%.0f°F" windChillF


mkColorFormatting :: ColorText -> (String, String)
mkColorFormatting NoColorChange = ("", "")
mkColorFormatting (ColorText colText) = ( printf "%%{F%s}" colText, "%{F-}")
