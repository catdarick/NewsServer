{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Methods.Helper where

import           Api.Types.Response
import           Data.List          (find)
import           Data.Maybe         (isJust, isNothing)
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           System.Random      (Random (randomRIO))

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
    missingArguments = foldr1 ((<>) . (<> ", ")) textArgs
    textArgs = map decodeUtf8 byteStringArgs
    byteStringArgs = filter f paramNames
    f v = isNothing $ find (\(name, val) -> name == v && isJust val) queryString

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

genToken :: IO [Char]
genToken = genRandomString 50