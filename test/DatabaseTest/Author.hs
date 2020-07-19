{-# LANGUAGE OverloadedStrings #-}

module DatabaseTest.Author where

import           Api.Types.Author
import           Api.Types.User
import           Control.Monad
import           Control.Monad.Trans.Class      (MonadTrans (lift))
import           Data.Function                  ((&))
import           Data.Time                      (Day (ModifiedJulianDay),
                                                 LocalTime (LocalTime),
                                                 midnight)
import           Database.Create.Author
import           Database.Create.User
import           Database.Delete.Author
import           Database.Delete.User
import           Database.Edit.Author
import           Database.Get.Author
import           Database.Get.User
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Transact   (getConnection)
import           Migration.Create
import           Test.Hspec                     (Spec, SpecWith, hspec)
import           Test.Hspec.DB
import           Test.Hspec.Expectations.Lifted


spec :: Spec
spec =
  describeDB initDatabase "Author: " $ do
    insert
    getById
    getByBadId
    getByUserId
    getByBadUserId
    getByLogin
    getByBadLogin
    getByFName
    getByBadFName
    getByLName
    getByBadLName
    edit
    delete

insert :: SpecWith TestDB
insert =
  itDB "can insert (author)" $ do
    conn <- getConnection
    [Only id] <- addTestUser conn
    addTestAuthor conn
    author <-
      lift $
      getAuthors conn Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    (withDefTime <$> author) `shouldBe` [testAuthor]

getById :: SpecWith TestDB
getById =
  itDB "can get by id" $ do
    conn <- getConnection
    author <-
      lift $
      getAuthors conn (Just 1) Nothing Nothing Nothing Nothing Nothing Nothing
    (withDefTime <$> author) `shouldBe` [testAuthor]

getByBadId :: SpecWith TestDB
getByBadId =
  itDB "get empty with incorrect id" $ do
    conn <- getConnection
    author <-
      lift $
      getAuthors conn (Just 2) Nothing Nothing Nothing Nothing Nothing Nothing
    (withDefTime <$> author) `shouldBe` []

getByUserId :: SpecWith TestDB
getByUserId =
  itDB "can get by user id" $ do
    conn <- getConnection
    author <-
      lift $
      getAuthors conn Nothing (Just 1) Nothing Nothing Nothing Nothing Nothing
    (withDefTime <$> author) `shouldBe` [testAuthor]

getByBadUserId :: SpecWith TestDB
getByBadUserId =
  itDB "get empty with incorrect user id" $ do
    conn <- getConnection
    author <-
      lift $
      getAuthors conn Nothing (Just 2) Nothing Nothing Nothing Nothing Nothing
    (withDefTime <$> author) `shouldBe` []

getByLogin :: SpecWith TestDB
getByLogin =
  itDB "can get by login" $ do
    conn <- getConnection
    author <-
      lift $
      getAuthors
        conn
        Nothing
        Nothing
        (Just "testLogin")
        Nothing
        Nothing
        Nothing
        Nothing
    (withDefTime <$> author) `shouldBe` [testAuthor]

getByBadLogin :: SpecWith TestDB
getByBadLogin =
  itDB "get empty with incorrect login" $ do
    conn <- getConnection
    author <-
      lift $
      getAuthors
        conn
        Nothing
        Nothing
        (Just "badLogin")
        Nothing
        Nothing
        Nothing
        Nothing
    (withDefTime <$> author) `shouldBe` []

getByFName :: SpecWith TestDB
getByFName =
  itDB "can get by first name" $ do
    conn <- getConnection
    author <-
      lift $
      getAuthors
        conn
        Nothing
        Nothing
        Nothing
        (Just "testName")
        Nothing
        Nothing
        Nothing
    (withDefTime <$> author) `shouldBe` [testAuthor]

getByBadFName :: SpecWith TestDB
getByBadFName =
  itDB "get empty with incorrect first name" $ do
    conn <- getConnection
    author <-
      lift $
      getAuthors
        conn
        Nothing
        Nothing
        Nothing
        (Just "badName")
        Nothing
        Nothing
        Nothing
    (withDefTime <$> author) `shouldBe` []

getByLName :: SpecWith TestDB
getByLName =
  itDB "can get by last name" $ do
    conn <- getConnection
    author <-
      lift $
      getAuthors
        conn
        Nothing
        Nothing
        Nothing
        Nothing
        (Just "testLName")
        Nothing
        Nothing
    (withDefTime <$> author) `shouldBe` [testAuthor]

getByBadLName :: SpecWith TestDB
getByBadLName =
  itDB "get empty with incorrect last name" $ do
    conn <- getConnection
    author <-
      lift $
      getAuthors
        conn
        Nothing
        Nothing
        Nothing
        Nothing
        (Just "badLName")
        Nothing
        Nothing
    (withDefTime <$> author) `shouldBe` []

edit =
  itDB "edit" $ do
    conn <- getConnection
    lift $ editAuthor conn 1 (Just "newTestDescription")
    author <-
      lift $
      getAuthors conn (Just 1) Nothing Nothing Nothing Nothing Nothing Nothing
    (withDefTime <$> author) `shouldBe` [editedAuthor]

delete :: SpecWith TestDB
delete =
  itDB "delete" $ do
    conn <- getConnection
    amount <- lift $ deleteAuthor conn 1
    amount `shouldBe` 1


defTime = LocalTime (ModifiedJulianDay 0) midnight

testUser = User 1 "testLogin" "testName" "testLName" Nothing defTime False

testAuthor = Author 1 testUser (Just "testDescription")

editedAuthor = Author 1 testUser (Just "newTestDescription")

withDefTime author@Author {authorUser = user} =
  author {authorUser = user {userCreationTime = defTime}}

addTestUser conn =
  lift $ addUser conn "testLogin" "" "testName" "testLName" Nothing False

addTestAuthor conn = lift $ addAuthor conn 1 (Just "testDescription")