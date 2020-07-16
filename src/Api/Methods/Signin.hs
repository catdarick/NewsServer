{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Methods.Signin where

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
import           Database.PostgreSQL.Simple (Connection)
import           Database.Types
import qualified Database.User              as DB
import           GHC.Exception              (errorCallException, throw)
import           Network.HTTP.Types.Status
import Database.PostgreSQL.Simple.Types (Only(Only))

signIn ::
     Connection -> [(ByteString, Maybe ByteString)] -> IO (Status, Response Idcont)
signIn conn queryString = do
  let eitherParameters = checkAndGetParameters required optional queryString
  case eitherParameters of
    Left error -> return (status400, errorResponse error)
    Right (requiredValues, optionalMaybeValues) -> do
      let [login, password, fName, lName] = requiredValues
      let [adminPass, picture] = optionalMaybeValues
      let passHash = (hash password)
      print passHash
      res <-
        try $
        DB.addUser
          conn
          login
          passHash
          fName
          lName
          picture
          (isJust adminPass)
      case res of
        Left (e :: SomeException) ->
          return (status400, errorResponse Err.loginBusy)
        Right [] -> return $ (status400, errorResponse Err.smth)
        Right [Only id] -> return (status200, idResponse id)
  where
    requiredNames = ["login", "password", "first_name", "last_name"]
    requiredChecks =
      [isCorrectLength 5 20, isCorrectLength 6 40, isNotEmpty, isNotEmpty]
    required = (requiredNames, requiredChecks)
    optionalNames = ["admin_pass", "picture"]
    optionalChecks = [isGlobalAdminPass, isNotEmpty]
    optional = (optionalNames, optionalChecks)
