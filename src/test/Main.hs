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
  ]
