{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Helpers.Check where

import           Api.Types.Response
import           Control.Exception     (throw)
import           Data.ByteString       (ByteString, length)
import           Data.ByteString.Char8 (pack)
import           Data.ByteString.Char8 (unpack)
import           Data.List             (find)
import           Data.Maybe            (fromJust, fromMaybe, isJust, isNothing)
import           Data.Text.Encoding    (decodeUtf8, encodeUtf8)
import           Database.Types
import           System.Random         (Random (randomRIO))
import           Text.Read             (readMaybe)

checkRequiredParamsForCorrect ::
     [(a, b) -> Either ByteString Bool] -> [(a, b)] -> Either ByteString [Bool]
checkRequiredParamsForCorrect predicats values = do
  let eitherList = zipWith id predicats values
  sequence eitherList

checkOptionalParamsForCorrect predicats values = do
  let eitherList = zipWith f predicats values
  sequence eitherList
  where
    f pred (a, (Just x)) = pred (a, x)
    f pred (_, Nothing)  = Right True

isNotEmpty :: (ByteString, ByteString) -> Either ByteString Bool
isNotEmpty (name, "") = Left $ name <> " field is empty."
isNotEmpty smt        = Right True

isNotEmptyTextList :: (ByteString, ByteString) -> Either ByteString Bool
isNotEmptyTextList (name, val)
  | isNothing maybeList = Left $ name <> " is not string list"
  | isInList (== "") = Left $ name <> " contains empty field"
  | otherwise = Right True
  where
    strVal = unpack val
    maybeList :: Maybe [String]
    maybeList = readMaybe strVal
    listString = fromJust maybeList
    isInList pred =
      case find pred listString of
        Nothing -> False
        Just _  -> True

isInt :: (ByteString, ByteString) -> Either ByteString Bool
isInt (name, val)
  | isNothing maybeInteger = Left $ name <> " is not decimal value"
  | fromJust maybeInteger > 2147483646 = Left $ name <> " is too large"
  | fromJust maybeInteger <= 0 = Left $ name <> " must be positive"
  | otherwise = Right True
  where
    strVal = unpack val
    maybeInteger :: Maybe Integer
    maybeInteger = readMaybe strVal

isIntList :: (ByteString, ByteString) -> Either ByteString Bool
isIntList (name, val)
  | isNothing maybeList = Left $ name <> " is not decimal list"
  | isInList (> 2147483646) = Left $ name <> " had too large value"
  | isInList (<= 0) = Left $ name <> " had negative value"
  | otherwise = Right True
  where
    strVal = unpack val
    maybeList :: Maybe [Integer]
    maybeList = readMaybe strVal
    listInteger = fromJust maybeList
    isInList pred =
      case find pred listInteger of
        Nothing -> False
        Just _  -> True

isBool :: (ByteString, ByteString) -> Either ByteString Bool
isBool (_, "true")  = Right True
isBool (_, "false") = Right True
isBool (name, val)  = Left $ name <> " is not bool."

isCorrectLength ::
     Int -> Int -> (ByteString, ByteString) -> Either ByteString Bool
isCorrectLength min max (name, str)
  | Data.ByteString.length str < min =
    Left $ name <> " is too short. Minimal length is " <> bsMin
  | Data.ByteString.length str > max =
    Left $ name <> " is too long. Maximal length is " <> bsMax
  | otherwise = Right True
  where
    bsMin = pack $ show min
    bsMax = pack $ show max
