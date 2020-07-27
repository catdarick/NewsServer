{-# LANGUAGE OverloadedStrings #-}

module Config where

import           Data.ByteString   (ByteString)
import           Data.Configurator (require)
import qualified Logger.Types      as Log

data Config =
  Config
    { dbHost        :: ByteString
    , dbPort        :: ByteString
    , dbName        :: ByteString
    , dbUsername    :: ByteString
    , dbUserPass    :: ByteString
    , logPath       :: String
    , logSinceLevel :: Log.Level
    }

parseConfig cfg = do
  dbHost <- require cfg "dbHost"
  dbPort <- require cfg "dbPort"
  dbName <- require cfg "dbName"
  dbUsername <- require cfg "dbUsername"
  dbUserPass <- require cfg "dbUserPass"
  logPath <- require cfg "logPath"
  logSinceLevel <- require cfg "logSinceLevel"
  return
    Config
      { dbHost = dbHost
      , dbPort = dbPort
      , dbName = dbName
      , dbUsername = dbUsername
      , dbUserPass = dbUserPass
      , logPath = logPath
      , logSinceLevel = logSinceLevel
      }
