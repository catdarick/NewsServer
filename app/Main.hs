{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Config
import           Control.Exception          (SomeException, try)
import           Data.Configurator          (load)
import           Data.Configurator.Types    (Worth (Required))
import           Data.Function              ((&))
import qualified Database.Create.User       as DB
import qualified Database.Init              as DB
import           Database.PostgreSQL.Simple (connectPostgreSQL)
import           Network.Wai.Handler.Warp   (run)
import           RequestHandler
import           System.Environment         (getArgs)

connectDatabaseAndDoAction :: Config -> IO ()
connectDatabaseAndDoAction config = do
  conn <- connectPostgreSQL $ connectString config
  args <- getArgs
  case args of
    ["-i"] -> init conn
    ["-a", login, pass, fName, lName] -> createAdmin conn login pass fName lName
    _ -> run 3000 (handleRequest conn config)
  where
    init conn = DB.init conn >> print "Tables created"
    createAdmin conn login pass fName lName = do
      DB.addAdmin conn login pass fName lName
      print "Admin created"
    connectString config =
      "host='" <>
      (config & dbHost) <>
      "' port=" <>
      (config & dbPort) <>
      " dbname='" <>
      (config & dbName) <>
      "' user='" <>
      (config & dbUsername) <> "' password='" <> (config & dbUserPass) <> "'"

main :: IO ()
main = do
  eitherCfg <- try $ load [Required "$(HOME)/configs/server.cfg"]
  case eitherCfg of
    Left (e :: SomeException) -> print "Can't open config file" >> print e
    Right handleConfig -> do
      config <- parseConfig handleConfig
      case checkConfig config of
        Left badField ->
          print $ "The field must be set in the config file: " ++ badField
        Right _ -> connectDatabaseAndDoAction config
