{-# LANGUAGE OverloadedStrings #-}

module MethodsTest.User where

import           Api.ErrorException
import qualified Api.Methods.Errors             as Err
import           MethodsTest.Helper

import           Api.Methods.Create.Account
import           Api.Methods.Delete.User
import           Api.Methods.Get.Token
import           Api.Methods.Get.User
import           Api.Types.Response
import           Api.Types.Response
import           Api.Types.User
import           Config
import           Control.Exception              (try)
import           Control.Monad
import           Control.Monad.Trans.Class      (MonadTrans (lift))
import           Data.ByteString.Char8          (ByteString, pack)
import           Data.Configurator              (load)
import           Data.Configurator.Types        (Worth (Required))
import           Data.Function                  ((&))
import           Data.Maybe                     (fromJust)
import           Data.Time.Calendar             (Day (ModifiedJulianDay))
import           Data.Time.LocalTime            (LocalTime (LocalTime),
                                                 midnight)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Transact   (DBT, getConnection)
import           Migration.Create
import           Network.HTTP.Types.Status      (status200, status400,
                                                 status404)
import           Test.Hspec                     (Spec, SpecWith, hspec)
import           Test.Hspec.DB
import           Test.Hspec.Expectations.Lifted

spec :: Spec
spec =
  describeDB initDatabase "Methods.User: " $ do
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
    config <- getConfig
    resp <- lift $ createAccount conn config query
    (resp & responseSuccess) `shouldBe` True
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
    config <- getConfig
    resp <- lift $ createAccount conn config query
    (resp & responseSuccess) `shouldBe` True
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
    config <- getConfig
    resp <- lift $ createAccount conn config query
    (resp & responseSuccess) `shouldBe` True
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
    config <- getConfig
    resp <- lift $ createAccount conn config (query config)
    (resp & responseSuccess) `shouldBe` True
  where
    query config =
      [ ("login", Just "admin")
      , ("password", Just "testPass")
      , ("first_name", Just "adminFName")
      , ("last_name", Just "adminLName")
      , ("admin_pass", Just $ config & globalAdminPass)
      ]

createUserDuplicate :: SpecWith TestDB
createUserDuplicate =
  itDB "can't create user with duplicate login" $ do
    conn <- getConnection
    config <- getConfig
    res <- lift $ try $ createAccount conn config query
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
    config <- getConfig
    res <- lift $ try $ createAccount conn config query
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
    config <- getConfig
    res <- lift $ try $ createAccount conn config query
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
    config <- getConfig
    res <- lift $ try $ createAccount conn config query
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
    config <- getConfig
    res <- lift $ try $ createAccount conn config query
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
    config <- getConfig
    res <- lift $ try $ createAccount conn config (query config)
    withEmptyError res `shouldBe` (Left $ ErrorException status400 "")
  where
    query config =
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
    (status, resp) <- lift $ getUsers conn query
    (status, withDefTime_ (resp & responseResult)) `shouldBe`
      (status200, Just [testUser, testAdmin])
  where
    query = []

getToken_ :: SpecWith TestDB
getToken_ =
  itDB "can get token" $ do
    conn <- getConnection
    (status, resp) <- lift $ getToken conn query
    (status, resp & responseSuccess) `shouldBe` (status200, True)
  where
    query = [("login", Just "user1"), ("password", Just "testPass")]

getById :: SpecWith TestDB
getById =
  itDB "can get by id" $ do
    conn <- getConnection
    (status, resp) <- lift $ getUsers conn query
    (status, withDefTime_ (resp & responseResult)) `shouldBe`
      (status200, Just [testUser])
  where
    query = [("user_id", Just "1")]

getByLogin :: SpecWith TestDB
getByLogin =
  itDB "can get by login" $ do
    conn <- getConnection
    (status, resp) <- lift $ getUsers conn query
    (status, withDefTime_ (resp & responseResult)) `shouldBe`
      (status200, Just [testUser])
  where
    query = [("login", Just "user1")]

getByFName :: SpecWith TestDB
getByFName =
  itDB "can get by first name" $ do
    conn <- getConnection
    (status, resp) <- lift $ getUsers conn query
    (status, withDefTime_ (resp & responseResult)) `shouldBe`
      (status200, Just [testUser])
  where
    query = [("first_name", Just "userFName")]

getByLName :: SpecWith TestDB
getByLName =
  itDB "can get by first name" $ do
    conn <- getConnection
    (status, resp) <- lift $ getUsers conn query
    (status, withDefTime_ (resp & responseResult)) `shouldBe`
      (status200, Just [testUser])
  where
    query = [("last_name", Just "userLName")]

deleteByUser :: SpecWith TestDB
deleteByUser =
  itDB "user can't delete account" $ do
    conn <- getConnection
    token <- getUserToken conn
    res <- lift $ try $ deleteUser conn (query token)
    res `shouldBe` (Left $ ErrorException status404 "")
  where
    query token = [("user_id", Just "2"), ("token", Just token)]

deleteByAdmin :: SpecWith TestDB
deleteByAdmin =
  itDB "admin can delete account" $ do
    conn <- getConnection
    token <- getAdminToken conn
    resp <- lift $ deleteUser conn (query token)
    (resp & responseSuccess) `shouldBe` True
  where
    query token = [("user_id", Just "1"), ("token", Just token)]

getUsersAfterDelete :: SpecWith TestDB
getUsersAfterDelete =
  itDB "can get only one account after delete (admin)" $ do
    conn <- getConnection
    (status, resp) <- lift $ getUsers conn query
    (status, withDefTime_ (resp & responseResult)) `shouldBe`
      (status200, Just [testAdmin])
  where
    query = []

getUserToken :: Connection -> DBT IO ByteString
getUserToken conn = do
  (_, respToken) <- lift $ getToken conn query
  return $ pack $ fromJust $respToken & responseResult
  where
    query = [("login", Just "user1"), ("password", Just "testPass")]

getAdminToken :: Connection -> DBT IO ByteString
getAdminToken conn = do
  (_, respToken) <- lift $ getToken conn query
  return $ pack $ fromJust $respToken & responseResult
  where
    query = [("login", Just "admin"), ("password", Just "testPass")]

getAuthor1Token :: Connection -> DBT IO ByteString
getAuthor1Token conn = do
  (_, respToken) <- lift $ getToken conn query
  return $ pack $ fromJust $respToken & responseResult
  where
    query = [("login", Just "author1"), ("password", Just "testPass")]

getAuthor2Token :: Connection -> DBT IO ByteString
getAuthor2Token conn = do
  (_, respToken) <- lift $ getToken conn query
  return $ pack $ fromJust $respToken & responseResult
  where
    query = [("login", Just "author2"), ("password", Just "testPass")]

defTime = (LocalTime (ModifiedJulianDay 0) midnight)

withDefTime user = user {userCreationTime = defTime}

withDefTime_ = (fmap . fmap) withDefTime

testUser = User 1 "user1" "userFName" "userLName" Nothing defTime False

testAdmin = User 2 "admin" "adminFName" "adminLName" Nothing defTime True

author1 = User 3 "author1" "author1FName" "author1LName" Nothing defTime False

author2 = User 4 "author2" "author2FName" "author2LName" Nothing defTime False

getConfig = do
  cfgHandle <- lift $ load [Required "$(PWD)/app/server.cfg"]
  lift $ parseConfig cfgHandle
