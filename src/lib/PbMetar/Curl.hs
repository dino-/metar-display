module PbMetar.Curl
  ( getMetar
  )
  where

import Network.Curl (CurlCode (CurlOK), curlGetString)
import Text.Printf (printf)


getMetar :: IO (Either String String)
getMetar = do
  let urlPrefix = "ftp://tgftp.nws.noaa.gov/data/observations/metar/stations" :: String
  let station = "KRDU" :: String
  let url = printf "%s/%s.TXT" urlPrefix station
  resultToEither <$> curlGetString url []


resultToEither :: (CurlCode, String) -> Either String String
resultToEither (CurlOK, responseBody) = Right responseBody
resultToEither (curlCode, _) = Left . show $ curlCode
