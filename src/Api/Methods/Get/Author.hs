{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Methods.Get.Author where

import           Api.Helpers.Checks
import           Api.Helpers.Getters
import           Api.Types.Author
import           Api.Types.Response
import           Api.Types.Synonyms
import           Data.ByteString            (ByteString)
import qualified Database.Get.Author        as DB
import           Database.PostgreSQL.Simple (Connection)
import           Network.HTTP.Types         (Status, status200)

getAuthors ::
     Connection
  -> [(ByteString, Maybe ByteString)]
  -> IO (Status, Response [Author])
getAuthors conn queryString = do
  (requiredValues, optionalMaybeValues) <- parameters
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
    parameters = checkAndGetParameters required optional queryString
