{-# LANGUAGE OverloadedStrings #-}

module Database.Types where

import           Data.ByteString (ByteString, length)
import           Data.Text       (Text)

type Login = ByteString

type Password = ByteString

type PassHash = ByteString

type FirstName = ByteString

type LastName = ByteString

type Name = ByteString

type Picture = ByteString

type PictureId = Int

type UserId = Int

type AuthorId = Int

type NewsId = Int

type TagId = Int

type CommentId = Int

type Token = ByteString

type TokenString = String

type CheckPredicat = (ByteString, ByteString) -> Either ByteString Bool

type OptionalParam = Maybe ByteString

type RequiredParam = ByteString

type FieldName = ByteString

type Error = ByteString

type IsAdmin = Bool

type Description = ByteString

type Title = ByteString

type CategoryId = Int


type Content = ByteString
