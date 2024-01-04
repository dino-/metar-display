module PbMetar.Output
  ( mkPolybarLabel
  )
  where

import Data.List (intercalate)
import Data.Time.LocalTime (TimeOfDay (..), TimeZone, utcToLocalTimeOfDay)
import Text.Printf (printf)

import PbMetar.Model.Common
import PbMetar.Model.Options
import PbMetar.Model.Weather
import PbMetar.Math (calculateWindChill)


mkPolybarLabel :: TimeZone -> FontIndex -> ColorText -> Weather Metric -> String
mkPolybarLabel localZone (FontIndex fontIndex) colorText weatherM =
  let weatherI = convert weatherM :: Weather Imperial
      (_, (TimeOfDay localHour localMin _)) = utcToLocalTimeOfDay localZone $ timeUtc weatherI
      (colorBegin, colorEnd) = mkColorFormatting colorText
      tempDisplay = intercalate "/" [format $ temperature weatherI, format $ calculateWindChill weatherI]
  in printf "%%{T%d}\xe586%%{T-} %s%02d:%02d%s %%{T%d}\xf2c9%%{T-} %s%s%s %%{T%d}\xf72e%%{T-} %s%s%s"
        fontIndex colorBegin localHour localMin colorEnd
        fontIndex colorBegin tempDisplay colorEnd
        fontIndex colorBegin (format $ wind weatherI) colorEnd


mkColorFormatting :: ColorText -> (String, String)
mkColorFormatting NoColorChange = ("", "")
mkColorFormatting (ColorText colText) = ( printf "%%{F%s}" colText, "%{F-}")
