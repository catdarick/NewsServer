{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Methods.Signin where

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
import Api.Methods.Helper

signIn conn queryString = do
  let eitherValues =
        getRequiredParams
          ["login", "password", "first_name", "last_name"]
          queryString
  print eitherValues
  case eitherValues of
    Left error -> return $ (status400, encode $ errorResponse error)
    Right [login, password, fName, lName] -> do
      let passHash = (hash password)
      print passHash
      res <- try $ addUser conn login passHash fName lName "" False
      case res of
        Left (e :: SomeException) ->
          return (status400, encode $ errorResponse errBusy)
        Right _ -> return (status200, encode okReponse)
  where
    errBusy = "Login is busy, try another."
