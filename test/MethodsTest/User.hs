{-# LANGUAGE OverloadedStrings #-}

module MethodsTest.User where

import           Api.ErrorException
import qualified Api.Errors                     as Err
import           Api.Methods.Create.Account
import           Api.Methods.Delete.User
import           Api.Methods.Get.Token
import           Api.Methods.Get.User
import           Api.Types.Response
import           Api.Types.User
import           Control.Exception              (try)
import           Control.Monad
import           Control.Monad.Trans.Class      (MonadTrans (lift))
import           Crypto.Hash.MD5                (hash)
import           Data.ByteString.Char8          (ByteString, pack)
import           Data.Function                  ((&))
import           Data.Maybe                     (fromJust)
import           Data.Time.Calendar             (Day (ModifiedJulianDay))
import           Data.Time.LocalTime            (LocalTime (LocalTime),
                                                 midnight)
import           Database.Create.User
import qualified Database.Init                  as DB
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Transact   (DBT, getConnection)
import           Network.HTTP.Types.Status      (status200, status201,
                                                 status400, status404)
import           Test.Hspec                     (Spec, SpecWith, hspec)
import           Test.Hspec.DB
import           Test.Hspec.Expectations.Lifted
import           TestHelper

spec :: Spec
spec =
  describeDB DB.init "Methods.User: " $ do
    createUser
    createAdmin
    createUserDuplicate
    createAdminWithBadPass
    createMissingLogin
    createMissingPassword
    createMissingFName
    createMissingLName
    getUsers_
    getToken_
    getById
    getByLogin
    getByFName
    getByLName
    deleteByUser
    deleteByAdmin
    getUsersAfterDelete

createUser :: SpecWith TestDB
createUser =
  itDB "can create user" $ do
    conn <- getConnection
    (status, resp) <- lift $ runWithState conn $ createAccount query
    (status, (resp & responseSuccess)) `shouldBe` (status201, True)
  where
    query =
      [ ("login", Just "user1")
      , ("password", Just "testPass")
      , ("first_name", Just "userFName")
      , ("last_name", Just "userLName")
      ]

createAuthor1Account :: SpecWith TestDB
createAuthor1Account =
  itDB "can create author1 account" $ do
    conn <- getConnection
    (status, resp) <- lift $ runWithState conn $ createAccount query
    (status, (resp & responseSuccess)) `shouldBe` (status201, True)
  where
    query =
      [ ("login", Just "author1")
      , ("password", Just "testPass")
      , ("first_name", Just "author1FName")
      , ("last_name", Just "author1LName")
      ]

createAuthor2Account :: SpecWith TestDB
createAuthor2Account =
  itDB "can create author2 account" $ do
    conn <- getConnection
    (status, resp) <- lift $ runWithState conn $ createAccount query
    (status, (resp & responseSuccess)) `shouldBe` (status201, True)
  where
    query =
      [ ("login", Just "author2")
      , ("password", Just "testPass")
      , ("first_name", Just "author2FName")
      , ("last_name", Just "author2LName")
      ]

createUserAdminAuthorAccounts = do
  createUser
  createAdmin
  createAuthor1Account
  createAuthor2Account

createAdmin :: SpecWith TestDB
createAdmin =
  itDB "can create admin" $ do
    conn <- getConnection
    let hashPass = hash "testPass"
    res <-
      lift $
      runWithState conn $
      addUser "admin" hashPass "adminFName" "adminLName" Nothing True
    res `shouldBe` 2
  where
    query =
      [ ("login", Just "admin")
      , ("password", Just "testPass")
      , ("first_name", Just "adminFName")
      , ("last_name", Just "adminLName")
      ]

createUserDuplicate :: SpecWith TestDB
createUserDuplicate =
  itDB "can't create user with duplicate login" $ do
    conn <- getConnection
    res <- lift $ try $ runWithState conn $ createAccount query
    res `shouldBe` (Left $ ErrorException status400 Err.loginBusy)
  where
    query =
      [ ("login", Just "user1")
      , ("password", Just "testPass")
      , ("first_name", Just "userFName")
      , ("last_name", Just "userLName")
      ]

createMissingLogin :: SpecWith TestDB
createMissingLogin =
  itDB "can't create without required 'login' field" $ do
    conn <- getConnection
    res <- lift $ try $ runWithState conn $ createAccount query
    withEmptyError res `shouldBe` (Left $ ErrorException status400 "")
  where
    query =
      [ ("password", Just "testPass")
      , ("first_name", Just "userFName")
      , ("last_name", Just "userLName")
      ]

createMissingPassword :: SpecWith TestDB
createMissingPassword =
  itDB "can't create without required 'password' field" $ do
    conn <- getConnection
    res <- lift $ try $ runWithState conn $ createAccount query
    withEmptyError res `shouldBe` (Left $ ErrorException status400 "")
  where
    query =
      [ ("login", Just "user3")
      , ("first_name", Just "userFName")
      , ("last_name", Just "userLName")
      ]

createMissingFName :: SpecWith TestDB
createMissingFName =
  itDB "can't create without required 'first_name' field" $ do
    conn <- getConnection
    res <- lift $ try $ runWithState conn $ createAccount query
    withEmptyError res `shouldBe` (Left $ ErrorException status400 "")
  where
    query =
      [ ("login", Just "user3")
      , ("password", Just "testPass")
      , ("last_name", Just "userLName")
      ]

createMissingLName :: SpecWith TestDB
createMissingLName =
  itDB "can't create without required 'last_name' field" $ do
    conn <- getConnection
    res <- lift $ try $ runWithState conn $ createAccount query
    withEmptyError res `shouldBe` (Left $ ErrorException status400 "")
  where
    query =
      [ ("login", Just "user3")
      , ("password", Just "testPass")
      , ("first_name", Just "userFName")
      ]

createAdminWithBadPass :: SpecWith TestDB
createAdminWithBadPass =
  itDB "can't create admin with bad pass" $ do
    conn <- getConnection
    res <- lift $ try $ runWithState conn $ createAccount (query)
    withEmptyError res `shouldBe` (Left $ ErrorException status400 "")
  where
    query =
      [ ("login", Just "admin")
      , ("password", Just "testPass")
      , ("first_name", Just "adminFName")
      , ("last_name", Just "adminLName")
      , ("admin_pass", Just "incorrectPass")
      ]

getUsers_ :: SpecWith TestDB
getUsers_ =
  itDB "can get only two accounts (user and admin)" $ do
    conn <- getConnection
    (status, resp) <- lift $ runWithState conn $ getUsers query
    (withDefTime_ (resp & responseResult)) `shouldBe` Just [testUser, testAdmin]
  where
    query = []

getToken_ :: SpecWith TestDB
getToken_ =
  itDB "can get token" $ do
    conn <- getConnection
    (status, resp) <- lift $ runWithState conn $ getToken query
    (resp & responseSuccess) `shouldBe` True
  where
    query = [("login", Just "user1"), ("password", Just "testPass")]

getById :: SpecWith TestDB
getById =
  itDB "can get by id" $ do
    conn <- getConnection
    (status, resp) <- lift $ runWithState conn $ getUsers query
    (withDefTime_ (resp & responseResult)) `shouldBe` Just [testUser]
  where
    query = [("user_id", Just "1")]

getByLogin :: SpecWith TestDB
getByLogin =
  itDB "can get by login" $ do
    conn <- getConnection
    (status, resp) <- lift $ runWithState conn $ getUsers query
    (withDefTime_ (resp & responseResult)) `shouldBe` Just [testUser]
  where
    query = [("login", Just "user1")]

getByFName :: SpecWith TestDB
getByFName =
  itDB "can get by first name" $ do
    conn <- getConnection
    (status, resp) <- lift $ runWithState conn $ getUsers query
    (withDefTime_ (resp & responseResult)) `shouldBe` Just [testUser]
  where
    query = [("first_name", Just "userFName")]

getByLName :: SpecWith TestDB
getByLName =
  itDB "can get by first name" $ do
    conn <- getConnection
    (status, resp) <- lift $ runWithState conn $ getUsers query
    (withDefTime_ (resp & responseResult)) `shouldBe` Just [testUser]
  where
    query = [("last_name", Just "userLName")]

deleteByUser :: SpecWith TestDB
deleteByUser =
  itDB "user can't delete account" $ do
    conn <- getConnection
    token <- getUserToken conn
    res <- lift $ try $ runWithState conn $ deleteUser (query token)
    res `shouldBe` (Left $ ErrorException status404 "")
  where
    query token = [("user_id", Just "2"), ("token", Just token)]

deleteByAdmin :: SpecWith TestDB
deleteByAdmin =
  itDB "admin can delete account" $ do
    conn <- getConnection
    token <- getAdminToken conn
    (status, resp) <- lift $ runWithState conn $ deleteUser (query token)
    (resp & responseSuccess) `shouldBe` True
  where
    query token = [("user_id", Just "1"), ("token", Just token)]

getUsersAfterDelete :: SpecWith TestDB
getUsersAfterDelete =
  itDB "can get only one account after delete (admin)" $ do
    conn <- getConnection
    (status, resp) <- lift $ runWithState conn $ getUsers query
    (withDefTime_ (resp & responseResult)) `shouldBe` Just [testAdmin]
  where
    query = []

getUserToken :: Connection -> DBT IO ByteString
getUserToken conn = do
  (status, respToken) <- lift $ runWithState conn $ getToken query
  return $ pack $ fromJust $respToken & responseResult
  where
    query = [("login", Just "user1"), ("password", Just "testPass")]

getAdminToken :: Connection -> DBT IO ByteString
getAdminToken conn = do
  (status, respToken) <- lift $ runWithState conn $ getToken query
  return $ pack $ fromJust $respToken & responseResult
  where
    query = [("login", Just "admin"), ("password", Just "testPass")]

getAuthor1Token :: Connection -> DBT IO ByteString
getAuthor1Token conn = do
  (status, respToken) <- lift $ runWithState conn $ getToken query
  return $ pack $ fromJust $respToken & responseResult
  where
    query = [("login", Just "author1"), ("password", Just "testPass")]

getAuthor2Token :: Connection -> DBT IO ByteString
getAuthor2Token conn = do
  (status, respToken) <- lift $ runWithState conn $ getToken query
  return $ pack $ fromJust $respToken & responseResult
  where
    query = [("login", Just "author2"), ("password", Just "testPass")]

withDefTime :: User -> User
withDefTime user = user {userCreationTime = defTime}

withDefTime_ :: Maybe [User] -> Maybe [User]
withDefTime_ = (fmap . fmap) withDefTime

testUser :: User
testUser = User 1 "user1" "userFName" "userLName" Nothing defTime False

testAdmin :: User
testAdmin = User 2 "admin" "adminFName" "adminLName" Nothing defTime True

author1 :: User
author1 = User 3 "author1" "author1FName" "author1LName" Nothing defTime False

author2 :: User
author2 = User 4 "author2" "author2FName" "author2LName" Nothing defTime False
