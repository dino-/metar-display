import Colog.Simple
  ( LogAction, Message
  , (%)
  , logError, logInfo, logNotice
  , logTextStderr, msg, sev)
import Colog.Simple qualified
import Data.Text (pack)
import Data.Time.LocalTime (getCurrentTimeZone)
import System.Exit (exitFailure, exitSuccess)

import MetarDisplay.Curl (getMetar)
import MetarDisplay.Metar (isolateMetarLine, parse)
import MetarDisplay.Model.Options (Options (..), Verbosity (..))
import MetarDisplay.Monad (App, liftIO, out, runApp)
import MetarDisplay.Opts (parseOpts)
import MetarDisplay.Output (mkOutput)


main :: IO ()
main = do
  opts <- parseOpts
  localZone <- getCurrentTimeZone
  runApp (logger . optVerbosity $ opts) $ do
    logInfo "metar-display started"
    curlResult <- getMetar . optStation $ opts
    let eMetarString = isolateMetarLine =<< curlResult
    either (const $ pure ()) (logNotice . pack) eMetarString
    let parsed = parse =<< eMetarString
    either exitFail exitOk $ mkOutput (optTemplate opts) localZone =<< parsed


exitOk :: String -> App ()
exitOk output = do
  logInfo $ "stdout output string: " <> pack output
  out output
  logInfo "metar-display finished successfully"
  liftIO $ exitSuccess


exitFail :: String -> App ()
exitFail errorInfo = do
  logError $ "Error: " <> pack errorInfo
  out "ERROR! See log for more info"
  liftIO exitFailure


logger :: Verbosity -> LogAction IO Message
logger  Quiet         = mempty
logger (Verbose sev') = Colog.Simple.logger (sev % msg) logTextStderr sev'
