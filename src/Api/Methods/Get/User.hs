{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Methods.Get.User where

import           Api.Helpers.Checks
import           Api.Helpers.Getters
import           Api.Types.Response
import           Api.Types.Synonyms
import           Api.Types.User
import           Data.ByteString            (ByteString)
import qualified Database.Get.User          as DB
import           Database.PostgreSQL.Simple (Connection)
import           Network.HTTP.Types         (Status, status200)
import           State.Types
import qualified Logger.Interact            as Log

getUsers ::
     [(ByteString, Maybe ByteString)] -> ServerStateIO (Status, Response [User])
getUsers queryString = do
  (requiredValues, optionalMaybeValues) <- parameters
  let [] = requiredValues
  let [mbUserId, mbLogin, mbFName, mbLName, mbLimit, mbOffset] =
        optionalMaybeValues
  users <-
    DB.getUsers
      (toInt <$> mbUserId)
      mbLogin
      mbFName
      mbLName
      (toInt <$> mbLimit)
      (toInt <$> mbOffset)
  return (status200, payloadResponse users)
  where
    requiredNames = []
    requiredChecks = []
    required = (requiredNames, requiredChecks)
    optionalNames =
      ["user_id", "login", "first_name", "last_name", "limit", "offset"]
    optionalChecks =
      [isInt, isNotEmpty, isNotEmpty, isNotEmpty, isIntBetween 1 200]
    optional = (optionalNames, optionalChecks)
    parameters = checkAndGetParameters required optional queryString
