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

createAuthor ::
     Connection -> [(ByteString, Maybe ByteString)] -> IO (Response Idcont)
createAuthor conn queryString = do
  (requiredValues, optionalMaybeValues) <- parameters
  let [token, userId] = requiredValues
  let [description] = optionalMaybeValues
  DB.adminGuard conn token
  id <- DB.addAuthor conn (toInt userId) description
  return $ idResponse id
  where
    requiredNames = ["token", "user_id"]
    requiredChecks = [isNotEmpty, isInt]
    required = (requiredNames, requiredChecks)
    optionalNames = ["description"]
    optionalChecks = [isNotEmpty]
    optional = (optionalNames, optionalChecks)
    parameters = checkAndGetParameters404 required optional queryString
