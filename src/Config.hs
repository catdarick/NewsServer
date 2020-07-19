{-# LANGUAGE OverloadedStrings #-}

module Config where

import           Data.ByteString   (ByteString)
import           Data.Configurator (require)

data Config =
  Config
    { dbHost          :: ByteString
    , dbPort          :: ByteString
    , dbName          :: ByteString
    , dbUsername      :: ByteString
    , dbUserPass      :: ByteString
    , globalAdminPass :: ByteString
    }

parseConfig cfg = do
  dbHost <- require cfg "dbHost" :: IO ByteString
  dbPort <- require cfg "dbPort" :: IO ByteString
  dbName <- require cfg "dbName" :: IO ByteString
  dbUsername <- require cfg "dbUsername" :: IO ByteString
  dbUserPass <- require cfg "dbUserPass" :: IO ByteString
  globalAdminPass <- require cfg "globalAdminPass" :: IO ByteString
  return
    Config
      { dbHost = dbHost
      , dbPort = dbPort
      , dbName = dbName
      , dbUsername = dbUsername
      , dbUserPass = dbUserPass
      , globalAdminPass = globalAdminPass
      }
