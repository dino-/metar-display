module PbMetar.Curl
  ( getMetar
  )
  where

import Network.Curl (CurlCode (CurlOK), curlGetString)
import Text.Printf (printf)

import PbMetar.Model.Common (Station (..))


getMetar :: Station -> IO (Either String String)
getMetar (Station station) = do
  let urlPrefix = "ftp://tgftp.nws.noaa.gov/data/observations/metar/stations" :: String
  let url = printf "%s/%s.TXT" urlPrefix station
  resultToEither <$> curlGetString url []


resultToEither :: (CurlCode, String) -> Either String String
resultToEither (CurlOK, responseBody) = Right responseBody
resultToEither (curlCode, _) = Left . show $ curlCode
