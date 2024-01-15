module MetarDisplay.Output
  ( mkOutput
  )
  where

import Data.Text (unpack)
import Data.Time.LocalTime (TimeOfDay (..), TimeZone, utcToLocalTimeOfDay)
import Text.Mustache ((~>), checkedSubstitute, compileTemplate, object)

import MetarDisplay.Model.Common (Imperial, Metric, convert)
import MetarDisplay.Model.Options (Template (..))
import MetarDisplay.Model.Weather (Weather (..), WindChill (..), hasChill)
import MetarDisplay.Model.Wind (hasGust)
import MetarDisplay.Math (calculateRelativeHumidity, calculateWindChill, formatTimeValue)


mkOutput :: Template -> TimeZone -> Weather Metric -> Either String String
mkOutput (Template templateString) localZone weatherM = do
  -- Convert, extract and compute various things from the metric weather we have
  let weatherI = convert weatherM :: Weather Imperial
  let (_, (TimeOfDay localHour localMin _)) = utcToLocalTimeOfDay localZone $ timeUtc weatherI
  let chillF = calculateWindChill weatherI

  -- Marshall the complete set of weather values for mustache template substitution
  let values = object
        [ "station" ~> station weatherM

        , "hour12" ~> (formatTimeValue $ mod localHour 12)
        , "hour24" ~> formatTimeValue localHour
        , "min" ~> localMin

        , "windKph" ~> wind weatherM
        , "windMph" ~> wind weatherI

        , "hasGust" ~> hasGust (gust weatherM)
        , "gustKph" ~> gust weatherM
        , "gustMph" ~> gust weatherI

        , "tempC" ~> temperature weatherM
        , "tempF" ~> temperature weatherI

        , "hasChill" ~> hasChill chillF
        , "chillC" ~> (convert chillF :: WindChill Metric)
        , "chillF" ~> chillF

        , "dewPointC" ~> dewPoint weatherM
        , "dewPointF" ~> dewPoint weatherI

        , "rh" ~> calculateRelativeHumidity (temperature weatherM) (dewPoint weatherM)
        ]

  template <- either (Left . show) Right $ compileTemplate "user-template" templateString
  unpack <$> (tupleToEither $ checkedSubstitute template values)


-- Helper to make the result of mustache's checked* functions into a monadic error
tupleToEither :: Show a => ([a], b) -> Either String b
tupleToEither ([], b) = Right b
tupleToEither (es, _) = Left . show $ es
