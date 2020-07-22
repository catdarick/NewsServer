{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Methods.Get.User where

import           Api.Helpers.Checks
import           Api.Helpers.Getters
import qualified Api.Methods.Errors               as Err
import           Api.Types
import           Api.Types.Response
import           Api.Types.User
import           Data.ByteString                  (ByteString)
import qualified Database.Get.User                as DB
import           Database.PostgreSQL.Simple       (Connection)
import           Database.PostgreSQL.Simple.Types (Only (Only))
import           Network.HTTP.Types.Status

getUsers ::
     Connection
  -> [(ByteString, Maybe ByteString)]
  -> IO (Status, Response [User])
getUsers conn queryString = do
  let eitherParameters = checkAndGetParametersEither required optional queryString
  case eitherParameters of
    Left error -> return (status400, errorResponse error)
    Right (requiredValues, optionalMaybeValues) -> do
      let [] = requiredValues
      let [mbUserId, mbLogin, mbFName, mbLName, mbLimit, mbOffset] =
            optionalMaybeValues
      users <-
        DB.getUsers
          conn
          (toInt <$> mbUserId)
          mbLogin
          mbFName
          mbLName
          (toInt <$> mbLimit)
          (toInt <$> mbOffset)
      return (status200, payloadResponse users)
  where
    requiredNames = []
    requiredChecks = [isInt]
    required = (requiredNames, requiredChecks)
    optionalNames =
      ["user_id", "login", "first_name", "last_name", "limit", "offset"]
    optionalChecks =
      [isInt, isNotEmpty, isNotEmpty, isNotEmpty, isIntBetween 1 200]
    optional = (optionalNames, optionalChecks)
