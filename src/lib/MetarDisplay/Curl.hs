module MetarDisplay.Curl
  ( getMetar
  )
  where

import Network.Curl (CurlCode (CurlOK), curlGetString)
import System.FilePath ((</>), (<.>))

import MetarDisplay.Log (infoM, lname)
import MetarDisplay.Model.Common (Station (..))


getMetar :: Station -> IO (Either String String)
getMetar (Station station) = do
  let urlPrefix = "ftp://tgftp.nws.noaa.gov/data/observations/metar/stations" :: String
  let url = urlPrefix </> station <.> "TXT"
  infoM lname $ "curl URL: " <> url
  curlResponse <- curlGetString url []
  infoM lname $ "raw curl response: " <> show curlResponse
  resultToEither <$> curlGetString url []


resultToEither :: (CurlCode, String) -> Either String String
resultToEither (CurlOK, responseBody) = Right responseBody
resultToEither (curlCode, _) = Left . show $ curlCode
