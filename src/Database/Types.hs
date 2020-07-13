module Database.Types where

import           Data.Text (Text)
import Data.ByteString (ByteString)

type Login = ByteString
type PassHash = ByteString
type FirstName = ByteString
type LastName = ByteString
type Picture = ByteString
type UserID = Int
type Token = String