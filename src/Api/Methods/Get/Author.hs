{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Methods.Get.Author where

import           Api.Helpers.Checks
import           Api.Helpers.Getters
import qualified Api.Methods.Errors               as Err
import           Api.Types
import           Api.Types.Author
import           Api.Types.Response
import           Data.ByteString                  (ByteString)
import qualified Database.Get.Author              as DB
import           Database.PostgreSQL.Simple       (Connection)
import           Database.PostgreSQL.Simple.Types (Only (Only))
import           Network.HTTP.Types.Status

getAuthors ::
     Connection
  -> [(ByteString, Maybe ByteString)]
  -> IO (Status, Response [Author])
getAuthors conn queryString = do
  let eitherParameters = checkAndGetParameters required optional queryString
  case eitherParameters of
    Left error -> return (status400, errorResponse error)
    Right (requiredValues, optionalMaybeValues) -> do
      let [] = requiredValues
      let [mbAuthorId, mbUserId, mbLogin, mbFName, mbLName, mbLimit, mbOffset] =
            optionalMaybeValues
      authors <-
        DB.getAuthors
          conn
          (toInt <$> mbAuthorId)
          (toInt <$> mbUserId)
          mbLogin
          mbFName
          mbLName
          (toInt <$> mbLimit)
          (toInt <$> mbOffset)
      return (status200, payloadResponse authors)
  where
    requiredNames = []
    requiredChecks = []
    required = (requiredNames, requiredChecks)
    optionalNames =
      [ "author_id"
      , "user_id"
      , "login"
      , "first_name"
      , "last_name"
      , "limit"
      , "offset"
      ]
    optionalChecks =
      [ isInt
      , isInt
      , isNotEmpty
      , isNotEmpty
      , isNotEmpty
      , isIntBetween 1 200
      , isInt
      ]
    optional = (optionalNames, optionalChecks)
