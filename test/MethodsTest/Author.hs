{-# LANGUAGE OverloadedStrings #-}

module MethodsTest.Author where

import           Api.Methods.Create.Account
import           Api.Methods.Create.Author
import           Api.Methods.Delete.Author
import           Api.Methods.Delete.User
import           Api.Methods.Get.Author
import           Api.Methods.Get.Token
import           Api.Methods.Get.User
import           Api.Types.Author
import           Api.Types.Response
import           Api.Types.User
import           Config
import           Control.Monad
import           Control.Monad.Trans.Class      (MonadTrans (lift))
import           Data.ByteString.Char8          (pack)
import           Data.Configurator              (load)
import           Data.Configurator.Types        (Worth (Required))
import           Data.Function                  ((&))
import           Data.Maybe                     (fromJust)
import           Data.Time.Calendar             (Day (ModifiedJulianDay))
import           Data.Time.LocalTime            (LocalTime (LocalTime),
                                                 midnight)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Transact   (getConnection)
import qualified MethodsTest.User               as User
import           Migration.Create
import           Network.HTTP.Types.Status      (status200, status400,
                                                 status404)
import           Test.Hspec                     (Spec, SpecWith, hspec)
import           Test.Hspec.DB
import           Test.Hspec.Expectations.Lifted

spec :: Spec
spec =
  describeDB initDatabase "Methods.Author: " $ do
    User.createUserAdminAuthorAccounts
    createAuthorByUser
    createAuthor1ByAdmin
    createAuthor2ByAdmin
    createMissingUserId
    createMissingToken
    getAuthors_
    getById
    getByUserId
    getByLogin
    getByFName
    getByLName
    deleteByUser
    deleteByAdmin
    getUsersAfterDelete

createAuthorByUser :: SpecWith TestDB
createAuthorByUser =
  itDB "user can't create author" $ do
    conn <- getConnection
    token <- User.getUserToken conn
    (status, resp) <- lift $ createAuthor conn (query token)
    (status, resp & responseSuccess) `shouldBe` (status404, False)
  where
    query token = [("user_id", Just "3"), ("token", Just token)]

createAuthor1ByAdmin :: SpecWith TestDB
createAuthor1ByAdmin =
  itDB "admin can create author" $ do
    conn <- getConnection
    token <- User.getAdminToken conn
    (status, resp) <- lift $ createAuthor conn (query token)
    (status, resp & responseSuccess) `shouldBe` (status200, True)
  where
    query token =
      [ ("user_id", Just "3")
      , ("token", Just token)
      , ("description", Just "someDescription")
      ]

createAuthor2ByAdmin :: SpecWith TestDB
createAuthor2ByAdmin =
  itDB "admin can make himsel an author" $ do
    conn <- getConnection
    token <- User.getAdminToken conn
    (status, resp) <- lift $ createAuthor conn (query token)
    (status, resp & responseSuccess) `shouldBe` (status200, True)
  where
    query token =
      [ ("user_id", Just "4")
      , ("token", Just token)
      , ("description", Just "someDescription2")
      ]

createMissingUserId :: SpecWith TestDB
createMissingUserId =
  itDB "admin can't create author without required 'user_id'" $ do
    conn <- getConnection
    token <- User.getAdminToken conn
    (status, resp) <- lift $ createAuthor conn (query token)
    (status, resp & responseSuccess) `shouldBe` (status404, False)
  where
    query token = [("token", Just token)]

createMissingToken :: SpecWith TestDB
createMissingToken =
  itDB "can't create author without required 'token'" $ do
    conn <- getConnection
    (status, resp) <- lift $ createAuthor conn query
    (status, resp & responseSuccess) `shouldBe` (status404, False)
  where
    query = [("user_id", Just "1")]

getAuthors_ :: SpecWith TestDB
getAuthors_ =
  itDB "get only two aauthors" $ do
    conn <- getConnection
    (status, resp) <- lift $ getAuthors conn query
    (fromJust $ withDefTime_ (resp & responseResult)) `shouldMatchList`
      [testAuthor1, testAuthor2]
  where
    query = []

getById :: SpecWith TestDB
getById =
  itDB "can get by id" $ do
    conn <- getConnection
    (status, resp) <- lift $ getAuthors conn query
    (status, withDefTime_ (resp & responseResult)) `shouldBe`
      (status200, Just [testAuthor1])
  where
    query = [("author_id", Just "1")]

getByUserId :: SpecWith TestDB
getByUserId =
  itDB "can get by user id" $ do
    conn <- getConnection
    (status, resp) <- lift $ getAuthors conn query
    (status, withDefTime_ (resp & responseResult)) `shouldBe`
      (status200, Just [testAuthor1])
  where
    query = [("user_id", Just "3")]

getByLogin :: SpecWith TestDB
getByLogin =
  itDB "can get by login" $ do
    conn <- getConnection
    (status, resp) <- lift $ getAuthors conn query
    (status, withDefTime_ (resp & responseResult)) `shouldBe`
      (status200, Just [testAuthor1])
  where
    query = [("login", Just "author1")]

getByFName :: SpecWith TestDB
getByFName =
  itDB "can get by first name" $ do
    conn <- getConnection
    (status, resp) <- lift $ getAuthors conn query
    (status, withDefTime_ (resp & responseResult)) `shouldBe`
      (status200, Just [testAuthor1])
  where
    query = [("first_name", Just "author1FName")]

getByLName :: SpecWith TestDB
getByLName =
  itDB "can get by first name" $ do
    conn <- getConnection
    (status, resp) <- lift $ getAuthors conn query
    (status, withDefTime_ (resp & responseResult)) `shouldBe`
      (status200, Just [testAuthor1])
  where
    query = [("last_name", Just "author1LName")]

deleteByUser :: SpecWith TestDB
deleteByUser =
  itDB "user can't delete author" $ do
    conn <- getConnection
    token <- User.getUserToken conn
    (status, resp) <- lift $ deleteAuthor conn (query token)
    (status, resp & responseSuccess) `shouldBe` (status404, False)
  where
    query token = [("author_id", Just "1"), ("token", Just token)]

deleteByAdmin :: SpecWith TestDB
deleteByAdmin =
  itDB "admin can delete account" $ do
    conn <- getConnection
    token <- User.getAdminToken conn
    (status, resp) <- lift $ deleteAuthor conn (query token)
    (status, resp & responseSuccess) `shouldBe` (status200, True)
  where
    query token = [("author_id", Just "1"), ("token", Just token)]

getUsersAfterDelete :: SpecWith TestDB
getUsersAfterDelete =
  itDB "get only one author after delete (author2)" $ do
    conn <- getConnection
    (status, resp) <- lift $ getAuthors conn query
    (status, withDefTime_ (resp & responseResult)) `shouldBe`
      (status200, Just [testAuthor2])
  where
    query = []

defTime = (LocalTime (ModifiedJulianDay 0) midnight)

withDefTime author@Author {authorUser = user} =
  author {authorUser = User.withDefTime user}

withDefTime_ = (fmap . fmap) withDefTime

testAuthor1 = Author 1 User.author1 (Just "someDescription")

testAuthor2 = Author 2 User.author2 (Just "someDescription2")
