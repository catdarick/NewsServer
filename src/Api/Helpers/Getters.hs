{-# LANGUAGE OverloadedStrings #-}

module Api.Helpers.Getters where

import           Api.Helpers.Checks
import           Api.ErrorException
import           Api.Types
import           Data.ByteString       (ByteString, length)
import           Data.ByteString.Char8 (unpack)
import           Data.List             (find)
import           Data.Maybe            (fromMaybe, isJust, isNothing)
import           Data.Text.Encoding    (decodeUtf8)
import           Data.Time.LocalTime   (LocalTime)
import           System.Random         (Random (randomRIO))
import Control.Monad.Catch (MonadThrow(throwM))
import Network.HTTP.Types (status404, status400)
checkAndGetParameters required optional query = do
  let res =  checkAndGetParametersEither required optional query
  case res of
    Left error -> throwM $ ErrorException status400 error
    Right values -> return values
checkAndGetParameters404 required optional query = do
  let res =  checkAndGetParametersEither required optional query
  case res of
    Left error -> throwM $ ErrorException status404 ""
    Right values -> return values
checkAndGetParametersEither ::
     ([FieldName], [CheckPredicat])
  -> ([FieldName], [CheckPredicat])
  -> [(FieldName, Maybe ByteString)]
  -> Either Error ([RequiredParam], [OptionalParam])
checkAndGetParametersEither (requiredNames, requiredChecks) (optionalNames, optionalChecks) queryString = do
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
genToken = genRandomString 50

toBool :: ByteString -> Bool
toBool "true"  = True
toBool "false" = False

toInt :: ByteString -> Int
toInt val = read $ unpack val

toIntList :: ByteString -> [Int]
toIntList val = read $ unpack val

toStringList :: ByteString -> [ByteString]
toStringList val = read $ unpack val

toDate :: ByteString -> LocalTime
toDate val = read dateWithTimeStr
  where
    dateWithTimeStr = unpack $ val <> " 00:00:00"
