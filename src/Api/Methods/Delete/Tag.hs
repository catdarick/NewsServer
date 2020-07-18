{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Methods.Delete.Tag where

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
import qualified Database.Tag                     as DB
import           Database.Types
import qualified Database.User                    as DB
import           Network.HTTP.Types               (Status, status200, status400,
                                                   status404)

deleteTag ::
     Connection -> [(ByteString, Maybe Login)] -> IO (Status, Response Idcont)
deleteTag conn queryString = do
  let eitherParameters = checkAndGetParameters required optional queryString
  case eitherParameters of
    Left error -> return (status404, badResoponse)
    Right (requiredValues, optionalMaybeValues) -> do
      let [token, tagId] = requiredValues
      let [] = optionalMaybeValues
      isAdmin <- DB.isAdminToken conn token
      if not isAdmin
        then return (status404, badResoponse)
        else do
          res <- try $ DB.deleteTag conn (fromInt tagId)
          case res of
            Left (e :: SomeException) ->
              return (status400, errorResponse Err.smth)
            Right 0 -> return $ (status400, errorResponse Err.noTag)
            Right 1 -> return $ (status200, okResponse)
  where
    requiredNames = ["token", "tag_id"]
    requiredChecks = [isNotEmpty, isInt]
    required = (requiredNames, requiredChecks)
    optionalNames = []
    optionalChecks = []
    optional = (optionalNames, optionalChecks)