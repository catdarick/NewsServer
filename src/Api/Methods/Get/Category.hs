{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Methods.Get.Category where

import           Api.Helpers.Check
import           Api.Helpers.Getters
import qualified Api.Methods.Errors               as Err
import           Api.Types.Category
import           Api.Types.Response
import           Control.Exception                (try)
import           Control.Exception                (SomeException)
import           Crypto.Hash.MD5                  (hash)
import           Data.Aeson                       (encode)
import           Data.ByteString                  (ByteString)
import           Data.List                        (find)
import           Data.Maybe                       (isJust, isNothing)
import           Data.Text.Encoding               (decodeUtf8, encodeUtf8)
import qualified Database.Category                as DB
import qualified Database.Get.Category            as DB
import           Database.PostgreSQL.Simple       (Connection)
import           Database.PostgreSQL.Simple.Types (Only (Only))
import           Database.Types
import           GHC.Exception                    (errorCallException, throw)
import           Network.HTTP.Types.Status

getCategories ::
     Connection
  -> [(ByteString, Maybe ByteString)]
  -> IO (Status, Response [Category])
getCategories conn queryString = do
  let eitherParameters = checkAndGetParameters required optional queryString
  case eitherParameters of
    Left error -> return (status400, errorResponse error)
    Right (requiredValues, optionalMaybeValues) -> do
      let [] = requiredValues
      let [categoryId, parentId, name] = optionalMaybeValues
      categories <-
        if isJust categoryId || isJust parentId || isJust name
          then DB.getCategoriesTree
                 conn
                 (fromInt <$> categoryId)
                 (fromInt <$> parentId)
                 name
          else DB.getRootCategoriesTree conn
      return (status200, payloadResponse categories)
  where
    requiredNames = []
    requiredChecks = [isInt]
    required = (requiredNames, requiredChecks)
    optionalNames = ["category_id", "parent_id", "name"]
    optionalChecks = [isInt, isInt, isNotEmpty]
    optional = (optionalNames, optionalChecks)
