module MetarDisplay.Log
  ( initLogging, lname, out

   -- Re-exported from System.Log
  , Priority (..)
  , debugM, infoM, noticeM, warningM, errorM , criticalM, alertM, emergencyM
  )
   where

import System.IO (stderr, stdout)
import System.Log.Formatter (simpleLogFormatter)
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple (streamHandler)
import System.Log.Logger

import MetarDisplay.Model.Options (LogDate (..), Verbosity (..))


lname :: String
lname = "logger"

outputLoggerName :: String
outputLoggerName = "output"


-- Function to use for normal stdout output
out :: String -> IO ()
out = infoM outputLoggerName


initLogging :: LogDate -> Verbosity -> IO ()
initLogging (LogDate logDate) verbosity = do
  -- Remove the root logger's default handler that writes every
  -- message to stderr!
  updateGlobalLogger rootLoggerName removeHandler
  updateGlobalLogger rootLoggerName $ setLevel DEBUG

  let formatString = if logDate
        then "[$time : $prio] $msg"
        else "[$prio] $msg"

  -- The logger for the actual log of this software, goes to stderr
  case verbosity of
    Quiet -> pure ()
    (Verbose priority) ->
      (flip setFormatter $ simpleLogFormatter formatString)
        <$> streamHandler stderr priority
        >>= updateGlobalLogger lname . addHandler

  -- The logger for normal stdout output of this software
  streamHandler stdout DEBUG >>= (updateGlobalLogger outputLoggerName . addHandler)
