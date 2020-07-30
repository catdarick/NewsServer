{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Methods.Get.Token where

import           Api.Helpers.Checks
import           Api.Helpers.Getters
import           Api.Types.Response
import           Api.Types.Synonyms
import           Crypto.Hash.MD5            (hash)
import           Data.ByteString            (ByteString)
import qualified Database.Create.User       as DB
import qualified Database.Get.User          as DB
import           Database.PostgreSQL.Simple (Connection)
import           Network.HTTP.Types         (Status, status200)
import           State.Types
import qualified Logger.Interact            as Log

getToken ::
     [(ByteString, Maybe Login)] -> ServerStateIO (Status, Response TokenString)
getToken queryString = do
  (requiredValues, optionalMaybeValues) <- parameters
  let [login, password] = requiredValues
  let passHash = hash password
  id <- DB.getUserIdByPass login passHash
  token <- genToken
  DB.setToken id token
  return (status200, payloadResponse token)
  where
    requiredNames = ["login", "password"]
    requiredChecks = [isCorrectLength 5 20, isCorrectLength 6 40]
    required = (requiredNames, requiredChecks)
    optionalNames = []
    optionalChecks = []
    optional = (optionalNames, optionalChecks)
    parameters = checkAndGetParameters required optional queryString
