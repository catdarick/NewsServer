{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Methods.Login where

import           Api.Methods.Helper
import           Api.Types.Response
import           Control.Exception         (try)
import           Control.Exception         (SomeException)
import           Crypto.Hash.MD5           (hash)
import           Data.Aeson                (encode)
import           Data.List                 (find)
import           Data.Maybe                (isJust, isNothing)
import           Data.Text.Encoding        (decodeUtf8, encodeUtf8)
import           Database.User
import           GHC.Exception             (errorCallException, throw)
import           Network.HTTP.Types.Status
import Database.PostgreSQL.Simple (Only(Only))

logIn conn queryString = do
  let eitherValues =
        getRequiredParams
          ["login", "password"]
          queryString
  print eitherValues
  case eitherValues of
    Left error -> return $ (status400, encode $ errorResponse error)
    Right [login, password] -> do
      let passHash = (hash password)
      print passHash
      res <- try $ getMaybeUserId conn login passHash
      case res of
        Left (e :: SomeException) ->
         return (status400, encode $ errorResponse err)
        Right [] ->  return (status400, encode $ errorResponse errIncorrectPass)
        Right [Only id] -> do 
            token <- genToken
            setToken conn id token
            return $ (status200, encode okReponse)
  where
    err = "Something goes wrong."
    errIncorrectPass = "Incorrect login or password"
