{-# LANGUAGE OverloadedStrings #-}

module Config where

import           Data.ByteString   (ByteString)
import           Data.Configurator (require)
import qualified Logger.Types      as Log
import qualified Data.Configurator.Types as Lib
import Data.Function ((&))
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

parseConfig :: Lib.Config -> IO Config
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

checkConfig :: Config -> Either String ()
checkConfig config
  | (config & dbHost) == "***" = Left "dbHost"
  | (config & dbPort) == "***" = Left "dbPort"
  | (config & dbName) == "***" = Left "dbName"
  | (config & dbUsername) == "***" = Left "dbUsername"
  | (config & dbUserPass) == "***" = Left "dbUserPass"
  | otherwise = Right ()
