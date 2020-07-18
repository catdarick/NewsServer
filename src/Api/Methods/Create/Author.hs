{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Methods.Create.Author where

import           Api.Helpers.Check
import           Api.Helpers.Getters
import qualified Api.Methods.Errors               as Err
import           Api.Types.Response
import           Control.Exception                (SomeException, try)
import           Data.ByteString                  (ByteString)
import           Data.ByteString.Char8            (unpack)
import qualified Database.Author                  as DB
import           Database.PostgreSQL.Simple       (Connection)
import           Database.PostgreSQL.Simple.Types (Only (Only))
import           Database.Types
import qualified Database.User                    as DB
import           Network.HTTP.Types               (Status, status200, status400,
                                                   status404)

createAuthor ::
     Connection -> [(ByteString, Maybe Login)] -> IO (Status, Response Idcont)
createAuthor conn queryString = do
  let eitherParameters = checkAndGetParameters required optional queryString
  case eitherParameters of
    Left error -> return (status404, badResoponse)
    Right (requiredValues, optionalMaybeValues) -> do
      let [token, userId] = requiredValues
      let [description] = optionalMaybeValues
      isAdmin <- DB.isAdminToken conn token
      if not isAdmin
        then return (status404, badResoponse)
        else do
          res <- try $ DB.addAuthor conn (fromInt userId) description
          case res of
            Left (e :: SomeException) -> do
              print e
              return (status400, errorResponse Err.alreadyAuthor)
            Right [] -> return $ (status400, errorResponse Err.smth)
            Right [Only id] -> return $ (status200, idResponse id)
  where
    requiredNames = ["token", "user_id"]
    requiredChecks = [isNotEmpty, isInt]
    required = (requiredNames, requiredChecks)
    optionalNames = ["description"]
    optionalChecks = [isNotEmpty]
    optional = (optionalNames, optionalChecks)