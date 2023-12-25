import Network.Curl (CurlCode (CurlOK), curlGetString)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)


main :: IO ()
main = do
  let urlPrefix = "ftp://tgftp.nws.noaa.gov/data/observations/metar/stations" :: String
  let station = "KRDU" :: String
  -- let station = "FOO" :: String
  let url = printf "%s/%s.TXT" urlPrefix station
  result <- resultToEither <$> curlGetString url []
  either (exitFail . show) exitOk result


resultToEither :: (CurlCode, String) -> Either CurlCode String
resultToEither (CurlOK, responseBody) = Right responseBody
resultToEither (curlCode, _) = Left curlCode


exitOk :: String -> IO ()
exitOk label = do
  putStrLn label
  exitSuccess


exitFail :: String -> IO ()
exitFail errorInfo = do
  hPutStrLn stderr $ printf "Error: %s" errorInfo
  exitFailure


celsiusToFahrenheit :: Fractional a => a -> a
celsiusToFahrenheit c = c * 1.8 + 32.0
