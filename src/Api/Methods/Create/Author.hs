{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Methods.Create.Author where

import           Api.Helpers.Checks
import           Api.Helpers.Getters
import           Api.Types.Response
import           Api.Types.Synonyms
import           Data.ByteString            (ByteString)
import qualified Database.Checks.User       as DB
import qualified Database.Create.Author     as DB
import           Database.PostgreSQL.Simple (Connection)
import qualified Logger.Interact            as Log
import           Network.HTTP.Types         (Status, status201)
import           State.Types

createAuthor ::
     [(ByteString, Maybe ByteString)] -> ServerStateIO (Status, Response Idcont)
createAuthor queryString = do
  (requiredValues, optionalMaybeValues) <- parameters
  let [token, userId] = requiredValues
  let [description] = optionalMaybeValues
  DB.adminGuard token
  id <- DB.addAuthor (toInt userId) description
  Log.info $ "User '" <> userId <> "' granted author permissions"
  return (status201, idResponse id)
  where
    requiredNames = ["token", "user_id"]
    requiredChecks = [isNotEmpty, isInt]
    required = (requiredNames, requiredChecks)
    optionalNames = ["description"]
    optionalChecks = [isNotEmpty]
    optional = (optionalNames, optionalChecks)
    parameters = checkAndGetParameters404 required optional queryString
