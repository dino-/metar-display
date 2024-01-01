import Data.Time.LocalTime (TimeOfDay (..), utc)
import PbMetar.Common
import PbMetar.Metar (parse)
import Test.Tasty
import Test.Tasty.HUnit


main :: IO ()
main = defaultMain $ testGroup " tests"
  [ testCase "A basic parse" $
      (Right (Weather (Observations {time = TimeOfDay 1 51 0, windKts = WindKts 3.0, tempC = TempCelsius 3.9, windMph = WindMph 3.452272727272727, tempF = TempFahr 39.019999999999996}) (WindChill (TempCelsius 2.078275458631617) (TempFahr 35.74089582553691)))) @=? (parse utc "KRDU 310151Z 23003KT 10SM FEW055 04/M02 A3000 RMK AO2 SLP160 T00391022")

  , testCase "Wind speed using 3 digits: 230103KT" $
      (Right (Weather (Observations {time = TimeOfDay 1 51 0, windKts = WindKts 3.0, tempC = TempCelsius 3.9, windMph = WindMph 3.452272727272727, tempF = TempFahr 39.019999999999996}) (WindChill (TempCelsius 2.078275458631617) (TempFahr 35.74089582553691)))) @=? (parse utc "KRDU 310151Z 23003KT 10SM FEW055 04/M02 A3000 RMK AO2 SLP160 T00391022")

  , testCase "Variable wind using 2 digits: VRB06KT" $
      (Right (Weather (Observations {time = TimeOfDay 0 51 0, windKts = WindKts 6.0, tempC = TempCelsius 6.7, windMph = WindMph 6.904545454545454, tempF = TempFahr 44.06}) (WindChill (TempCelsius 3.934145631032959) (TempFahr 39.081462135859326)))) @=? (parse utc "KRDU 300051Z VRB06KT 10SM FEW038 BKN050 OVC095 07/00 A2982 RMK AO2 SLP098 T00670000")

  , testCase "Variable wind using 3 digits: VRB106KT" $
      (Right (Weather (Observations {time = TimeOfDay 0 51 0, windKts = WindKts 106.0, tempC = TempCelsius 6.7, windMph = WindMph 121.98030303030303, tempF = TempFahr 44.06}) (WindChill (TempCelsius (-3.5318245926359877)) (TempFahr 25.642715733255223)))) @=? (parse utc "KRDU 300051Z VRB106KT 10SM FEW038 BKN050 OVC095 07/00 A2982 RMK AO2 SLP098 T00670000")

  , testCase "Wind gust using 2 digits: 28008G17KT" $
      (Right (Weather (Observations {time = TimeOfDay 16 51 0, windKts = WindKts 8.0, tempC = TempCelsius 9.4, windMph = WindMph 9.206060606060607, tempF = TempFahr 48.92}) (WindChill (TempCelsius 6.655666833513599) (TempFahr 43.980200300324476)))) @=? (parse utc "KRDU 301651Z 28008G17KT 10SM FEW047 SCT065 09/M02 A2992 RMK AO2 SLP134 T00941022")

  , testCase "Wind gust using 3 digits: 28008G107KT" $
      (Right (Weather (Observations {time = TimeOfDay 16 51 0, windKts = WindKts 8.0, tempC = TempCelsius 9.4, windMph = WindMph 9.206060606060607, tempF = TempFahr 48.92}) (WindChill (TempCelsius 6.655666833513599) (TempFahr 43.980200300324476)))) @=? (parse utc "KRDU 301651Z 28008G17KT 10SM FEW047 SCT065 09/M02 A2992 RMK AO2 SLP134 T00941022")

  , testCase "Temperature data only in 05/04 format" $
      (Right (Weather (Observations {time = TimeOfDay 3 50 0, windKts = WindKts 8.0, tempC = TempCelsius 5.0, windMph = WindMph 9.206060606060607, tempF = TempFahr 41.0}) (WindChill (TempCelsius 1.237943671685855) (TempFahr 34.22829860903454)))) @=? (parse utc "EKCH 310350Z AUTO 15008KT 9999 OVC032/// 05/04 Q1009 NOSIG")

  , testCase "Temperature data only in 05/M04 format" $
      (Right (Weather (Observations {time = TimeOfDay 3 50 0, windKts = WindKts 8.0, tempC = TempCelsius 5.0, windMph = WindMph 9.206060606060607, tempF = TempFahr 41.0}) (WindChill (TempCelsius 1.237943671685855) (TempFahr 34.22829860903454)))) @=? (parse utc "EKCH 310350Z AUTO 15008KT 9999 OVC032/// 05/M04 Q1009 NOSIG")

  , testCase "Temperature data only in M05/04 format" $
      (Right (Weather (Observations {time = TimeOfDay 3 50 0, windKts = WindKts 8.0, tempC = TempCelsius (-5.0), windMph = WindMph 9.206060606060607, tempF = TempFahr 23.0}) (WindChill (TempCelsius (-11.075063514286292)) (TempFahr 12.064885674284676)))) @=? (parse utc "EKCH 310350Z AUTO 15008KT 9999 OVC032/// M05/04 Q1009 NOSIG")

  , testCase "Temperature data only in M02/M02 format" $
      (Right (Weather (Observations {time = TimeOfDay 3 50 0, windKts = WindKts 8.0, tempC = TempCelsius (-2.0), windMph = WindMph 9.206060606060607, tempF = TempFahr 28.4}) (WindChill (TempCelsius (-7.381161358494646)) (TempFahr 18.713909554709637)))) @=? (parse utc "EKCH 310350Z AUTO 15008KT 9999 OVC032/// M02/M02 Q1009 NOSIG")

  , testCase "Wind speed in MPS using 2 digits: 15004MPS" $
      (Right (Weather (Observations {time = TimeOfDay 3 50 0, windKts = WindKts 7.775378, tempC = TempCelsius 5.0, windMph = WindMph 8.947575137878788, tempF = TempFahr 41.0}) (WindChill (TempCelsius 1.303595981550032) (TempFahr 34.34647276679006)))) @=? (parse utc "EKCH 310350Z AUTO 15004MPS 9999 OVC032/// 05/04 Q1009 NOSIG")

  , testCase "Wind speed in MPS using 3 digits: 150004MPS" $
      (Right (Weather (Observations {time = TimeOfDay 3 50 0, windKts = WindKts 7.775378, tempC = TempCelsius 5.0, windMph = WindMph 8.947575137878788, tempF = TempFahr 41.0}) (WindChill (TempCelsius 1.303595981550032) (TempFahr 34.34647276679006)))) @=? (parse utc "EKCH 310350Z AUTO 150004MPS 9999 OVC032/// 05/04 Q1009 NOSIG")

  , testCase "No wind info at all" $
      (Right (Weather (Observations {time = TimeOfDay 20 51 0, windKts = WindKts 0.0, tempC = TempCelsius 9.4, windMph = WindMph 0.0, tempF = TempFahr 48.92}) NoEffect)) @=? (parse utc "KRDU 012051Z 10SM SCT040 SCT200 BKN250 09/M02 A3004 RMK AO2 SLP174 T00941022 53007")
  ]
