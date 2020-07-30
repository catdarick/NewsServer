{-# LANGUAGE ScopedTypeVariables #-}

module Logger.Interact where

import           Config
import           Control.Exception         (SomeException, catch, try)
import           Control.Monad             (when)
import           Control.Monad.Trans.Class (MonadTrans (lift))
import           Control.Monad.Trans.State (gets, modify)
import           Data.ByteString           (ByteString)
import           Data.ByteString.Char8     (pack, unpack)
import           Data.Function             ((&))
import           Data.Time                 (getCurrentTime)
import           GHC.Exception.Type        (SomeException (SomeException))
import           Logger.Types
import           State.Types
import           Text.Printf               (printf)

appendLog :: String -> String -> ServerStateIO ()
appendLog lvl msg = do
  time <- lift getCurrentTime
  clientInfo <- gets clientInfo
  path <- gets $ logPath . config
  let logString =
        printf
          "%19.19s %22.22s %7.7s: %s\n"
          (show time)
          (show clientInfo)
          lvl
          msg
  lift $ appendFile (path ++ "server.log") logString
  lift $ print (lvl ++ ": " ++ msg ++ "\n")
  return ()

debug :: ByteString -> ServerStateIO ()
debug msg = do
  logSinceLevel <- gets $ logSinceLevel . config
  when (logSinceLevel <= DEBUG) $ appendLog "DEBUG" (unpack msg)

info :: ByteString -> ServerStateIO ()
info msg = do
  logSinceLevel <- gets $ logSinceLevel . config
  when (logSinceLevel <= INFO) $ appendLog "INFO" (unpack msg)

warn :: ByteString -> ServerStateIO ()
warn msg = do
  logSinceLevel <- gets $ logSinceLevel . config
  when (logSinceLevel <= WARNING) $ appendLog "WARNING" (unpack msg)

withErrorLogging :: Monoid b => IO b -> ServerStateIO b
withErrorLogging f = do
  res <- lift (try f)
  case res of
    Left (e :: SomeException) -> do
      warn $ pack (show e)
      return mempty
    Right x -> return x
