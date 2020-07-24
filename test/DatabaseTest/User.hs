{-# LANGUAGE OverloadedStrings #-}

module DatabaseTest.User where

import           Api.Types.Synonyms
import           Api.Types.User
import           Control.Monad
import           Control.Monad.Trans.Class      (MonadTrans (lift))
import           Data.Function                  ((&))
import           Data.Time.Calendar             (Day (ModifiedJulianDay))
import           Data.Time.LocalTime            (LocalTime (LocalTime),
                                                 midnight)
import           Database.Create.User
import           Database.Delete.User
import           Database.Get.User
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Transact   (getConnection)
import           Migration.Create
import           Test.Hspec                     (Spec, SpecWith, hspec)
import           Test.Hspec.DB
import           Test.Hspec.Expectations.Lifted

spec :: Spec
spec =
  describeDB initDatabase "User: " $ do
    insert
    getById
    getByBadId
    getByLogin
    getByBadLogin
    getByFName
    getByBadFName
    getByLName
    getByBadLName
    delete

insert :: SpecWith TestDB
insert =
  itDB "can insert" $ do
    conn <- getConnection
    id <- lift $ addTestUser conn
    user <- lift $ getUsers conn Nothing Nothing Nothing Nothing Nothing Nothing
    (withDefTime <$> user) `shouldBe` [testUser]

getById :: SpecWith TestDB
getById =
  itDB "can get by id" $ do
    conn <- getConnection
    user <-
      lift $ getUsers conn (Just 1) Nothing Nothing Nothing Nothing Nothing
    (withDefTime <$> user) `shouldBe` [testUser]

getByBadId :: SpecWith TestDB
getByBadId =
  itDB "get empty with incorrect id" $ do
    conn <- getConnection
    user <-
      lift $ getUsers conn (Just 2) Nothing Nothing Nothing Nothing Nothing
    (withDefTime <$> user) `shouldBe` []

getByLogin :: SpecWith TestDB
getByLogin =
  itDB "can get by login" $ do
    conn <- getConnection
    user <-
      lift $
      getUsers conn Nothing (Just "testLogin") Nothing Nothing Nothing Nothing
    (withDefTime <$> user) `shouldBe` [testUser]

getByBadLogin :: SpecWith TestDB
getByBadLogin =
  itDB "get empty with incorrect login" $ do
    conn <- getConnection
    user <-
      lift $
      getUsers conn Nothing (Just "badLogin") Nothing Nothing Nothing Nothing
    (withDefTime <$> user) `shouldBe` []

getByFName :: SpecWith TestDB
getByFName =
  itDB "can get by first name" $ do
    conn <- getConnection
    user <-
      lift $
      getUsers conn Nothing Nothing (Just "testName") Nothing Nothing Nothing
    (withDefTime <$> user) `shouldBe` [testUser]

getByBadFName :: SpecWith TestDB
getByBadFName =
  itDB "get empty with incorrect first name" $ do
    conn <- getConnection
    user <-
      lift $
      getUsers conn Nothing Nothing (Just "badName") Nothing Nothing Nothing
    (withDefTime <$> user) `shouldBe` []

getByLName :: SpecWith TestDB
getByLName =
  itDB "can get by last name" $ do
    conn <- getConnection
    user <-
      lift $
      getUsers conn Nothing Nothing Nothing (Just "testLName") Nothing Nothing
    (withDefTime <$> user) `shouldBe` [testUser]

getByBadLName :: SpecWith TestDB
getByBadLName =
  itDB "get empty with incorrect last name" $ do
    conn <- getConnection
    user <-
      lift $
      getUsers conn Nothing Nothing Nothing (Just "badLName") Nothing Nothing
    (withDefTime <$> user) `shouldBe` []

delete :: SpecWith TestDB
delete =
  itDB "delete" $ do
    conn <- getConnection
    res <- lift $ deleteUser conn 1
    res `shouldBe` ()

defTime :: LocalTime


testUser :: User
testUser = User 1 "testLogin" "testName" "testLName" Nothing defTime False

withDefTime :: User -> User
withDefTime user = user {userCreationTime = defTime}

addTestUser :: Connection -> IO UserId
addTestUser conn =
  addUser conn "testLogin" "" "testName" "testLName" Nothing False
