module PbMetar.Output
  ( mkOutput
  )
  where

import Data.Time.LocalTime (TimeOfDay (..), TimeZone, utcToLocalTimeOfDay)
import Text.Printf (printf)

import PbMetar.Model.Common
import PbMetar.Model.Options
import PbMetar.Model.Weather
import PbMetar.Math (calculateWindChill)


mkOutput :: OutputUnits -> Weather Metric -> TimeZone -> FontIndex -> ColorText -> String
mkOutput OUImperial = mkOutputI . convert
mkOutput OUMetric = mkOutputM

-- It's really crap that I can't do this with a single function that takes a (Weather system)

mkOutputI :: Weather Imperial -> TimeZone -> FontIndex -> ColorText -> String
mkOutputI weather localZone (FontIndex fontIndex) colorText =
  let (_, (TimeOfDay localHour localMin _)) = utcToLocalTimeOfDay localZone $ timeUtc weather
      (colorBegin, colorEnd) = mkColorFormatting colorText
      tempDisplay = combineTemps [format $ temperature weather, format $ calculateWindChill weather]
  in printf "%%{T%d}\xe586%%{T-} %s%02d:%02d%s %%{T%d}\xf2c9%%{T-} %s%s%s %%{T%d}\xf72e%%{T-} %s%s%s"
        fontIndex colorBegin localHour localMin colorEnd
        fontIndex colorBegin tempDisplay colorEnd
        fontIndex colorBegin (format $ wind weather) colorEnd


mkOutputM :: Weather Metric -> TimeZone -> FontIndex -> ColorText -> String
mkOutputM weather localZone (FontIndex fontIndex) colorText =
  let (_, (TimeOfDay localHour localMin _)) = utcToLocalTimeOfDay localZone $ timeUtc weather
      (colorBegin, colorEnd) = mkColorFormatting colorText
      windChillM = (convert . calculateWindChill . convert $ weather) :: WindChill Metric
      tempDisplay = combineTemps [format $ temperature weather, format windChillM]
  in printf "%%{T%d}\xe586%%{T-} %s%02d:%02d%s %%{T%d}\xf2c9%%{T-} %s%s%s %%{T%d}\xf72e%%{T-} %s%s%s"
        fontIndex colorBegin localHour localMin colorEnd
        fontIndex colorBegin tempDisplay colorEnd
        fontIndex colorBegin (format $ wind weather) colorEnd


combineTemps :: [String] -> String
combineTemps (t : "" : _) = t
combineTemps (t : u  : _) = t <> "/" <> u
combineTemps _            = undefined  -- Trust me, this will never happen


mkColorFormatting :: ColorText -> (String, String)
mkColorFormatting NoColorChange = ("", "")
mkColorFormatting (ColorText colText) = ( printf "%%{F%s}" colText, "%{F-}")
