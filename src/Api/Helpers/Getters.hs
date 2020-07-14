{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Helpers.Getters where

import           Api.Helpers.Check
import           Api.Types.Response
import           Control.Exception                  (Exception, SomeException, catch,
                                                     throw)
import           Data.ByteString                    (ByteString, length)
import           Data.ByteString.Char8              (unpack)
import           Data.ByteString.Char8              (pack)
import           Data.List                          (find)
import           Data.Maybe                         (fromMaybe, isJust,
                                                     isNothing)
import           Data.Text.Encoding                 (decodeUtf8, encodeUtf8)
import           Database.PostgreSQL.Simple         (connectPostgreSQL)
import           Database.PostgreSQL.Simple.SqlQQ   (sql)
import           Database.PostgreSQL.Simple.ToField (ToField (toField),
                                                     toJSONField)
import           Database.PostgreSQL.Simple.Types   (In (In))
import           Database.Types
import           Network.HTTP.Types.Status          (status404, Status)
import           System.Random                      (Random (randomRIO))

checkAndGetParameters ::
     ([FieldName], [CheckPredicat])
  -> ([FieldName], [CheckPredicat])
  -> [(FieldName, Maybe ByteString)]
  -> Either Error ([RequiredParam], [OptionalParam])
checkAndGetParameters (requiredNames, requiredChecks) (optionalNames, optionalChecks) queryString = do
  requiredValues <- getRequiredParams requiredNames queryString
  let optionalMaybeValues = getOptionalParams optionalNames queryString
  let requiredPair = zip requiredNames requiredValues
  let optionalPair = zip optionalNames optionalMaybeValues
  checkCorrect requiredPair optionalPair
  return (requiredValues, optionalMaybeValues)
  where
    checkCorrect requiredValues optionalValues = do
      checkRequiredParamsForCorrect requiredChecks requiredValues
      checkOptionalParamsForCorrect optionalChecks optionalValues

getRequiredParams :: [FieldName] -> [(FieldName, Maybe a)] -> Either Error [a]
getRequiredParams paramNames queryString = do
  let listOfMaybeMaybeValues = map ((flip lookup) queryString) paramNames
  let maybeListOfMaybeValues = sequence listOfMaybeMaybeValues
  case maybeListOfMaybeValues of
    Nothing -> Left $ "Missing required arguments: " <> missingArguments
    Just listOfMaybeValues -> do
      let maybeListOfValues = sequence listOfMaybeValues
      case maybeListOfValues of
        Nothing -> Left $ "Missing required arguments: " <> missingArguments
        Just listOfValues -> Right listOfValues --  map decodeUtf8 listOfValues
  where
    missingArguments = foldr1 ((<>) . (<> ", ")) byteStringArgs
    textArgs = map decodeUtf8 byteStringArgs
    byteStringArgs = filter f paramNames
    f v = isNothing $ find (\(name, val) -> name == v && isJust val) queryString

getOptionalParams ::
     [FieldName] -> [(FieldName, Maybe ByteString)] -> [Maybe ByteString]
getOptionalParams paramNames queryString = do
  let listOfMaybeMaybeValues = map ((flip lookup) queryString) paramNames
  let listOfMaybeValues = map (fromMaybe Nothing) listOfMaybeMaybeValues
  listOfMaybeValues

genRandomString :: Int -> IO [Char]
genRandomString 0 = return []
genRandomString length = do
  t <- randomRIO (1 :: Int, 3 :: Int)
  x <-
    case t of
      1 -> randomRIO ('a', 'z')
      2 -> randomRIO ('A', 'Z')
      3 -> randomRIO ('0', '9')
  xs <- genRandomString (length - 1)
  return $ x : xs

genToken :: IO TokenString
genToken = do
  r <- genRandomString 50
  return r

fromBool :: ByteString -> Bool
fromBool "true"  = True
fromBool "false" = False

fromInt :: ByteString -> Int
fromInt val = read $ unpack val

fromIntList :: ByteString -> [Int]
fromIntList val = read $ unpack val

fromStringList :: ByteString -> [ByteString]
fromStringList val = read $ unpack val


