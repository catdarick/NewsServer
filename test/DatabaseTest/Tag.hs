{-# LANGUAGE OverloadedStrings #-}

module DatabaseTest.Tag where

import           Api.Types.Tag
import           Control.Monad.Trans.Class      (MonadTrans (lift))
import           Data.Function                  ((&))
import           Data.Time.Calendar             (Day (ModifiedJulianDay))
import           Data.Time.LocalTime            (LocalTime (LocalTime),
                                                 midnight)
import           Database.Create.Tag
import           Database.Delete.Tag
import           Database.Edit.Tag
import           Database.Get.Tag
import qualified Database.Init                  as DB
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Transact   (getConnection)
import           Test.Hspec                     (Spec, SpecWith, hspec)
import           Test.Hspec.DB
import           Test.Hspec.Expectations.Lifted
import           TestHelper

spec :: Spec
spec =
  describeDB DB.init "Tag: " $ do
    insert
    getById
    getByBadId
    getByName
    getByBadName
    edit
    delete

insert :: SpecWith TestDB
insert =
  itDB "can insert (tag)" $ do
    conn <- getConnection
    lift $ runWithState conn $ addTag "testName"
    tag <-
      lift $ runWithState conn $ getTags Nothing Nothing Nothing Nothing Nothing
    tag `shouldBe` [testTag]

getById :: SpecWith TestDB
getById =
  itDB "can get by id" $ do
    conn <- getConnection
    tag <-
      lift $
      runWithState conn $ getTags (Just 1) Nothing Nothing Nothing Nothing
    tag `shouldBe` [testTag]

getByBadId :: SpecWith TestDB
getByBadId =
  itDB "get empty with incorrect id" $ do
    conn <- getConnection
    tag <-
      lift $
      runWithState conn $ getTags (Just 3) Nothing Nothing Nothing Nothing
    tag `shouldBe` []

getByName :: SpecWith TestDB
getByName =
  itDB "can get by name" $ do
    conn <- getConnection
    tag <-
      lift $
      runWithState conn $
      getTags Nothing Nothing (Just "testName") Nothing Nothing
    tag `shouldBe` [testTag]

getByBadName :: SpecWith TestDB
getByBadName =
  itDB "get empty with incorrect name" $ do
    conn <- getConnection
    tag <-
      lift $
      runWithState conn $
      getTags Nothing Nothing (Just "badName") Nothing Nothing
    tag `shouldBe` []

edit :: SpecWith TestDB
edit =
  itDB "edit" $ do
    conn <- getConnection
    lift $ runWithState conn $ editTag 1 (Just "newTestName")
    tag <-
      lift $ runWithState conn $ getTags Nothing Nothing Nothing Nothing Nothing
    tag `shouldBe` [testEditedTag]

delete :: SpecWith TestDB
delete =
  itDB "delete" $ do
    conn <- getConnection
    amount <- lift $ runWithState conn $ deleteTag 1
    amount `shouldBe` ()

testTag :: Tag
testTag = Tag 1 "testName"

testEditedTag :: Tag
testEditedTag = Tag 1 "newTestName"
