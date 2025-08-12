module MetarDisplay.Monad
  (
    App
  , runApp

  , out

    -- * Re-exporting
  , liftIO
  )
  where

import Colog (LoggerT, usingLoggerT)
import Colog.Simple (LogAction, Message)
import Control.Monad.Trans (liftIO)


type App a = LoggerT Message IO a


runApp :: LogAction IO Message -> App a -> IO a
runApp = usingLoggerT


out :: String -> App ()
out = liftIO . putStrLn
