{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Config
import           Control.Exception          (SomeException, try)
import           Data.Configurator          (load)
import           Data.Configurator.Types    (Worth (Required))
import           Data.Function              ((&))
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
    ["-i"] -> DB.init conn
    _      -> run 3000 (handleRequest conn config)
  where
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
  eitherCfg <- try $ load [Required "$(PWD)/app/server.cfg"]
  case eitherCfg of
    Left (e :: SomeException) -> print "Can't find config file" >> print e
    Right handleConfig -> do
      config <- parseConfig handleConfig
      connectDatabaseAndDoAction config
