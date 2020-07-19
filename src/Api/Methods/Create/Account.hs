{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Methods.Create.Account where

import           Api.Helpers.Checks
import           Api.Helpers.Getters
import qualified Api.Methods.Errors               as Err
import           Api.Types
import           Api.Types.Response
import           Config
import           Control.Exception                (SomeException, try)
import           Crypto.Hash.MD5                  (hash)
import           Data.ByteString                  (ByteString)
import           Data.Function                    ((&))
import           Data.Maybe                       (isJust, isNothing)
import qualified Database.Create.User             as DB
import           Database.PostgreSQL.Simple       (Connection)
import           Database.PostgreSQL.Simple.Types (Only (Only))
import           Network.HTTP.Types.Status

createAccount conn config queryString = do
  let eitherParameters = checkAndGetParameters required optional queryString
  case eitherParameters of
    Left error -> return (status400, errorResponse error)
    Right (requiredValues, optionalMaybeValues) -> do
      let [login, password, fName, lName] = requiredValues
      let [adminPass, picture] = optionalMaybeValues
      let passHash = hash password
      res <-
        try $
        DB.addUser conn login passHash fName lName picture (isJust adminPass)
      case res of
        Left (e :: SomeException) ->
          return (status400, errorResponse Err.loginBusy)
        Right [] -> return (status400, errorResponse Err.smth)
        Right [Only id] -> return (status200, idResponse id)
  where
    requiredNames = ["login", "password", "first_name", "last_name"]
    requiredChecks =
      [isCorrectLength 5 20, isCorrectLength 6 40, isNotEmpty, isNotEmpty]
    required = (requiredNames, requiredChecks)
    optionalNames = ["admin_pass", "picture"]
    optionalChecks = [isGlobalAdminPass (config & globalAdminPass), isNotEmpty]
    optional = (optionalNames, optionalChecks)
