module PbMetar.Log
  ( initLogging, lname, out

   -- Re-exported from System.Log
  , debugM, infoM, noticeM, warningM, errorM , criticalM, alertM, emergencyM
  , Priority (..)
  )
   where

import System.IO (stderr, stdout)
import System.Log.Formatter (simpleLogFormatter)
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple (streamHandler)
import System.Log.Logger


lname :: String
lname = "logger"

outputLoggerName :: String
outputLoggerName = "output"


-- Function to use for normal stdout output
out :: String -> IO ()
out = infoM outputLoggerName


initLogging :: Priority -> IO ()
initLogging priority = do
  -- Remove the root logger's default handler that writes every
  -- message to stderr!
  updateGlobalLogger rootLoggerName removeHandler
  updateGlobalLogger rootLoggerName $ setLevel DEBUG

  -- The logger for the actual log of this software, goes to stderr
  (flip setFormatter $ simpleLogFormatter "[$time : $prio] $msg")
    <$> streamHandler stderr priority
    >>= updateGlobalLogger lname . addHandler

  -- The logger for normal stdout output of this software, what polybar shows
  streamHandler stdout DEBUG >>= (updateGlobalLogger outputLoggerName . addHandler)
