import Data.Time.LocalTime (TimeOfDay (..))
import MetarDisplay.Metar (parse)
import MetarDisplay.Model.Common (Imperial, Metric, Nautical, Station (..), convert)
import MetarDisplay.Model.Temperature
import MetarDisplay.Model.Weather (Weather (..))
import MetarDisplay.Model.Wind (Gust (..), Wind (..))
import Test.Tasty
import Test.Tasty.HUnit


main :: IO ()
main = defaultMain $ testGroup " tests"
  [ conversion, parsing ]

conversion :: TestTree
conversion = testGroup "conversion"
  [ testCase "Weather metric to imperial conversion" $
      (Weather (Station "FOOO") (TimeOfDay 1 51 0) (Wind 11.104473600000002) (Gust (Wind 50.266250496000005)) (Temperature 0.5555555555555556) (Temperature (negate 2.2222222222222223)) :: Weather Metric)
        @=?
        convert ((Weather (Station "FOOO") (TimeOfDay 1 51 0) (Wind 6.9) (Gust (Wind 31.234)) (Temperature 33.0) (Temperature 28.0)) :: Weather Imperial)
  , testCase "Weather metric to imperial conversion" $
      (Weather (Station "FOOO") (TimeOfDay 1 51 0) (Wind 1.8641135699999998) (Gust (Wind 19.40790774846)) (Temperature 39.019999999999996) (Temperature 32.0) :: Weather Imperial)
        @=?
        convert ((Weather (Station "FOOO") (TimeOfDay 1 51 0) (Wind 3.0) (Gust (Wind 31.234)) (Temperature 3.9) (Temperature 0.0)) :: Weather Metric)
  , testCase "Wind nautical to metric conversion" $
      ((Wind 1.852) :: Wind Metric) @=?  convert ((Wind 1.0) :: Wind Nautical)
  ]

parsing :: TestTree
parsing = testGroup "parsing"
  [ testCase "A basic parse" $
      (Right (Weather {station = Station "KRDU", timeUtc = TimeOfDay 1 51 0, wind = Wind 5.556, gust = NoGust, temperature = Temperature 3.9, dewPoint = Temperature (negate 2.2)}))
        @=? (parse "KRDU 310151Z 23003KT 10SM FEW055 04/M02 A3000 RMK AO2 SLP160 T00391022")

  , testCase "Wind in knots using 3 digits: 230103KT" $
      (Right (Weather {station = Station "KRDU", timeUtc = TimeOfDay 1 51 0, wind = Wind 5.556, gust = NoGust, temperature = Temperature 3.9, dewPoint = Temperature (negate 2.2)}))
        @=? (parse "KRDU 310151Z 23003KT 10SM FEW055 04/M02 A3000 RMK AO2 SLP160 T00391022")

  , testCase "Variable wind using 2 digits: VRB06KT" $
      (Right (Weather {station = Station "KRDU", timeUtc = TimeOfDay 0 51 0, wind = Wind 11.112, gust = NoGust, temperature = Temperature 6.7, dewPoint = Temperature 0.0}))
        @=? (parse "KRDU 300051Z VRB06KT 10SM FEW038 BKN050 OVC095 07/00 A2982 RMK AO2 SLP098 T00670000")

  , testCase "Variable wind using 3 digits: VRB106KT" $
      (Right (Weather {station = Station "KRDU", timeUtc = TimeOfDay 0 51 0, wind = Wind 196.312, gust = NoGust, temperature = Temperature 6.7, dewPoint = Temperature 0.0}))
        @=? (parse "KRDU 300051Z VRB106KT 10SM FEW038 BKN050 OVC095 07/00 A2982 RMK AO2 SLP098 T00670000")

  , testCase "Wind with gust in knots using 2 digits: 28008G17KT" $
      (Right (Weather {station = Station "KRDU", timeUtc = TimeOfDay 16 51 0, wind = Wind 14.816, gust = Gust (Wind 31.484), temperature = Temperature 9.4, dewPoint = Temperature (negate 2.2)}))
        @=? (parse "KRDU 301651Z 28008G17KT 10SM FEW047 SCT065 09/M02 A2992 RMK AO2 SLP134 T00941022")

  , testCase "Wind with gust in knots using 3 digits: 28008G107KT" $
      (Right (Weather {station = Station "KRDU", timeUtc = TimeOfDay 16 51 0, wind = Wind 14.816, gust = Gust (Wind 198.16400000000002), temperature = Temperature 9.4, dewPoint = Temperature (negate 2.2)}))
        @=? (parse "KRDU 301651Z 28008G107KT 10SM FEW047 SCT065 09/M02 A2992 RMK AO2 SLP134 T00941022")

  , testCase "Temperature data only in 05/04 format" $
      (Right (Weather {station = Station "EKCH", timeUtc = TimeOfDay 3 50 0, wind = Wind 14.816, gust = NoGust, temperature = Temperature 5.0, dewPoint = Temperature 4.0}))
        @=? (parse "EKCH 310350Z AUTO 15008KT 9999 OVC032/// 05/04 Q1009 NOSIG")

  , testCase "Temperature data only in 05/M04 format" $
      (Right (Weather {station = Station "EKCH", timeUtc = TimeOfDay 3 50 0, wind = Wind 14.816, gust = NoGust, temperature = Temperature 5.0, dewPoint = Temperature (negate 4.0)}))
        @=? (parse "EKCH 310350Z AUTO 15008KT 9999 OVC032/// 05/M04 Q1009 NOSIG")

  , testCase "Temperature data only in M05/04 format" $
      (Right (Weather {station = Station "EKCH", timeUtc = TimeOfDay 3 50 0, wind = Wind 14.816, gust = NoGust, temperature = Temperature (negate 5.0), dewPoint = Temperature 4.0}))
        @=? (parse "EKCH 310350Z AUTO 15008KT 9999 OVC032/// M05/04 Q1009 NOSIG")

  , testCase "Temperature data only in M02/M02 format" $
      (Right (Weather {station = Station "EKCH", timeUtc = TimeOfDay 3 50 0, wind = Wind 14.816, gust = NoGust, temperature = Temperature (negate 2.0), dewPoint = Temperature (negate 2.0)}))
        @=? (parse "EKCH 310350Z AUTO 15008KT 9999 OVC032/// M02/M02 Q1009 NOSIG")

  , testCase "Wind in MPS using 2 digits: 15004MPS" $
      (Right (Weather {station = Station "EKCH", timeUtc = TimeOfDay 3 50 0, wind = Wind 14.4, gust = NoGust, temperature = Temperature 5.0, dewPoint = Temperature 4.0}))
        @=? (parse "EKCH 310350Z AUTO 15004MPS 9999 OVC032/// 05/04 Q1009 NOSIG")

  , testCase "Wind in MPS using 3 digits: 150004MPS" $
      (Right (Weather {station = Station "EKCH", timeUtc = TimeOfDay 3 50 0, wind = Wind 14.4, gust = NoGust, temperature = Temperature 5.0, dewPoint = Temperature 4.0}))
        @=? (parse "EKCH 310350Z AUTO 150004MPS 9999 OVC032/// 05/04 Q1009 NOSIG")

  , testCase "Wind in MPS with gust using 2 digits: 15004G12MPS" $
      (Right (Weather {station = Station "EKCH", timeUtc = TimeOfDay 3 50 0, wind = Wind 14.4, gust = Gust (Wind 43.2), temperature = Temperature 5.0, dewPoint = Temperature 4.0}))
        @=? (parse "EKCH 310350Z AUTO 15004G12MPS 9999 OVC032/// 05/04 Q1009 NOSIG")

  , testCase "Wind in MPS with gust using 3 digits: 150004G102MPS" $
      (Right (Weather {station = Station "EKCH", timeUtc = TimeOfDay 3 50 0, wind = Wind 14.4, gust = Gust (Wind 367.2), temperature = Temperature 5.0, dewPoint = Temperature 4.0}))
        @=? (parse "EKCH 310350Z AUTO 150004G102MPS 9999 OVC032/// 05/04 Q1009 NOSIG")

  , testCase "No wind info at all" $
      (Right (Weather {station = Station "KRDU", timeUtc = TimeOfDay 20 51 0, wind = Wind 0.0, gust = NoGust, temperature = Temperature 9.4, dewPoint = Temperature (negate 2.2)}))
        @=? (parse "KRDU 012051Z 10SM SCT040 SCT200 BKN250 09/M02 A3004 RMK AO2 SLP174 T00941022 53007")
  ]
