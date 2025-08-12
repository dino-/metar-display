module MetarDisplay.Curl
  ( getMetar
  )
  where

import Colog.Simple (logInfo)
import Data.Text (pack)
import Network.Curl (CurlCode (CurlOK), curlGetString)
import System.FilePath ((</>), (<.>))

import MetarDisplay.Model.Common (Station (..))
import MetarDisplay.Monad (App, liftIO)


getMetar :: Station -> App (Either String String)
getMetar (Station station) = do
  let urlPrefix = "ftp://tgftp.nws.noaa.gov/data/observations/metar/stations" :: String
  let url = urlPrefix </> station <.> "TXT"
  logInfo . pack $ "curl URL: " <> url
  curlResponse <- liftIO $ curlGetString url []
  logInfo . pack $ "raw curl response: " <> show curlResponse
  pure $ resultToEither curlResponse


resultToEither :: (CurlCode, String) -> Either String String
resultToEither (CurlOK, responseBody) = Right responseBody
resultToEither (curlCode, _) = Left . show $ curlCode
