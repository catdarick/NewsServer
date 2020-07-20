{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Methods.Get.Token where

import           Api.Helpers.Checks
import           Api.Helpers.Getters
import qualified Api.Methods.Errors         as Err
import           Api.Types
import           Api.Types.Response
import           Control.Exception          (SomeException, try)
import           Crypto.Hash.MD5            (hash)
import           Data.ByteString            (ByteString)
import qualified Database.Create.User       as DB
import qualified Database.Get.User          as DB
import           Database.PostgreSQL.Simple (Connection, Only (Only))
import           Network.HTTP.Types.Status

getToken ::
     Connection
  -> [(ByteString, Maybe Login)]
  -> IO (Status, Response TokenString)
getToken conn queryString = do
  let eitherParameters = checkAndGetParameters required optional queryString
  case eitherParameters of
    Left error -> return (status400, errorResponse error)
    Right (requiredValues, optionalMaybeValues) -> do
      let [login, password] = requiredValues
      let passHash = (hash password)
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
