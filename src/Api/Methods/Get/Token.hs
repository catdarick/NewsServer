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
  -> IO (Response TokenString)
getToken conn queryString = do
  (requiredValues, optionalMaybeValues) <- parameters
  let [login, password] = requiredValues
  let passHash = (hash password)
  id <- DB.getUserIdByPass conn login passHash
  token <- genToken
  DB.setToken conn id token
  return $ payloadResponse token
  where
    requiredNames = ["login", "password"]
    requiredChecks = [isCorrectLength 5 20, isCorrectLength 6 40]
    required = (requiredNames, requiredChecks)
    optionalNames = []
    optionalChecks = []
    optional = (optionalNames, optionalChecks)
    parameters = checkAndGetParameters required optional queryString
