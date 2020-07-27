{-# LANGUAGE OverloadedStrings #-}

module DatabaseTest.Author where

import           Api.Types.Author
import           Api.Types.Synonyms
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
import qualified Database.Init                  as DB
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Transact   (getConnection)
import           Test.Hspec                     (Spec, SpecWith, hspec)
import           Test.Hspec.DB
import           Test.Hspec.Expectations.Lifted
import           TestHelper

spec :: Spec
spec =
  describeDB DB.init "Author: " $ do
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
    addTestUser conn
    addTestAuthor conn
    author <-
      lift $
      runWithState conn $
      getAuthors Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    (withDefTime <$> author) `shouldBe` [testAuthor]

getById :: SpecWith TestDB
getById =
  itDB "can get by id" $ do
    conn <- getConnection
    author <-
      lift $
      runWithState conn $
      getAuthors (Just 1) Nothing Nothing Nothing Nothing Nothing Nothing
    (withDefTime <$> author) `shouldBe` [testAuthor]

getByBadId :: SpecWith TestDB
getByBadId =
  itDB "get empty with incorrect id" $ do
    conn <- getConnection
    author <-
      lift $
      runWithState conn $
      getAuthors (Just 2) Nothing Nothing Nothing Nothing Nothing Nothing
    (withDefTime <$> author) `shouldBe` []

getByUserId :: SpecWith TestDB
getByUserId =
  itDB "can get by user id" $ do
    conn <- getConnection
    author <-
      lift $
      runWithState conn $
      getAuthors Nothing (Just 1) Nothing Nothing Nothing Nothing Nothing
    (withDefTime <$> author) `shouldBe` [testAuthor]

getByBadUserId :: SpecWith TestDB
getByBadUserId =
  itDB "get empty with incorrect user id" $ do
    conn <- getConnection
    author <-
      lift $
      runWithState conn $
      getAuthors Nothing (Just 2) Nothing Nothing Nothing Nothing Nothing
    (withDefTime <$> author) `shouldBe` []

getByLogin :: SpecWith TestDB
getByLogin =
  itDB "can get by login" $ do
    conn <- getConnection
    author <-
      lift $
      runWithState conn $
      getAuthors
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
      runWithState conn $
      getAuthors
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
      runWithState conn $
      getAuthors
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
      runWithState conn $
      getAuthors
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
      runWithState conn $
      getAuthors
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
      runWithState conn $
      getAuthors
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
    lift $ runWithState conn $ editAuthor 1 (Just "newTestDescription")
    author <-
      lift $
      runWithState conn $
      getAuthors (Just 1) Nothing Nothing Nothing Nothing Nothing Nothing
    (withDefTime <$> author) `shouldBe` [editedAuthor]

delete :: SpecWith TestDB
delete =
  itDB "delete" $ do
    conn <- getConnection
    amount <- lift $ runWithState conn $ deleteAuthor 1
    amount `shouldBe` ()

testUser :: User
testUser = User 1 "testLogin" "testName" "testLName" Nothing defTime False

testAuthor :: Author
testAuthor = Author 1 testUser (Just "testDescription")

editedAuthor :: Author
editedAuthor = Author 1 testUser (Just "newTestDescription")

withDefTime :: Author -> Author
withDefTime author@Author {authorUser = user} =
  author {authorUser = user {userCreationTime = defTime}}

addTestUser :: MonadTrans t => Connection -> t IO UserId
addTestUser conn =
  lift $
  runWithState conn $
  addUser "testLogin" "" "testName" "testLName" Nothing False

addTestAuthor :: MonadTrans t => Connection -> t IO AuthorId
addTestAuthor conn =
  lift $ runWithState conn $ addAuthor 1 (Just "testDescription")
