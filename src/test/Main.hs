import Data.Time.LocalTime (TimeOfDay (..), utc)
import PbMetar.Common
import PbMetar.Metar (parse)
import Test.Tasty
import Test.Tasty.HUnit


main :: IO ()
main = defaultMain $ testGroup " tests"
  [ testCase "A basic parse" $
      (Right (Weather (Observations {time = TimeOfDay 1 51 0, windKts = WindKts 3.0, tempC = TempCelsius 3.9, windMph = WindMph 3.452272727272727, tempF = TempFahr 39.019999999999996}) (WindChill (TempCelsius 2.078275458631617) (TempFahr 35.74089582553691)))) @=? (parse utc "KRDU 310151Z 23003KT 10SM FEW055 04/M02 A3000 RMK AO2 SLP160 T00391022")

  , testCase "Variable wind indicated with VRB12KT" $
      (Right (Weather (Observations {time = TimeOfDay 0 51 0, windKts = WindKts 6.0, tempC = TempCelsius 6.7, windMph = WindMph 6.904545454545454, tempF = TempFahr 44.06}) (WindChill (TempCelsius 3.934145631032959) (TempFahr 39.081462135859326)))) @=? (parse utc "KRDU 300051Z VRB06KT 10SM FEW038 BKN050 OVC095 07/00 A2982 RMK AO2 SLP098 T00670000")
  ]
