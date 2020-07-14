{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Methods.Login where

import qualified Api.Methods.Errors         as Err
import           Api.Helpers.Getters
import           Api.Helpers.Check
import           Api.Types.Response
import           Control.Exception          (try)
import           Control.Exception          (SomeException)
import           Crypto.Hash.MD5            (hash)
import           Data.Aeson                 (encode)
import           Data.ByteString            (ByteString)
import           Data.List                  (find)
import           Data.Maybe                 (isJust, isNothing)
import           Data.Text.Encoding         (decodeUtf8, encodeUtf8)
import           Database.PostgreSQL.Simple (Connection, Only (Only))
import           Database.Types
import qualified Database.User              as DB
import           GHC.Exception              (errorCallException, throw)
import           Network.HTTP.Types.Status

logIn ::
     Connection -> [(ByteString, Maybe Login)] -> IO (Status, Response Token)
logIn conn queryString = do
  let eitherParameters = checkAndGetParameters required optional queryString
  case eitherParameters of
    Left error -> return (status400, errorResponse error)
    Right (requiredValues, optionalMaybeValues) -> do
      let [login, password] = requiredValues
      let passHash = (hash password)
      print passHash
      res <- try $ DB.getMaybeUserId conn login passHash
      case res of
        Left (e :: SomeException) -> return (status400, errorResponse Err.smth)
        Right [] -> return (status400, errorResponse Err.badPassword)
        Right [Only id] -> do
          token <- genToken
          DB.setToken conn id token
          let response = okResponseWithResult token
          return $ (status200, response)
  where
    requiredNames = ["login", "password"]
    requiredChecks = [isCorrectLength 5 20, isCorrectLength 6 40]
    required = (requiredNames, requiredChecks)
    optionalNames = []
    optionalChecks = []
    optional = (optionalNames, optionalChecks)
