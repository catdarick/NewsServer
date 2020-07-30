{-# LANGUAGE OverloadedStrings #-}

module DatabaseTest.Category where

import           Api.Types.Category
import           Control.Monad
import           Control.Monad.Trans.Class      (MonadTrans (lift))
import           Data.Function                  ((&))
import           Data.Time.Calendar             (Day (ModifiedJulianDay))
import           Data.Time.LocalTime            (LocalTime (LocalTime),
                                                 midnight)
import           Database.Create.Category
import           Database.Delete.Category
import           Database.Edit.Category
import           Database.Get.Category
import qualified Database.Init                  as DB
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Transact   (getConnection)
import           Test.Hspec                     (Spec, SpecWith, hspec)
import           Test.Hspec.DB
import           Test.Hspec.Expectations.Lifted
import           TestHelper

spec :: Spec
spec =
  describeDB DB.init "Category: " $ do
    insert
    getById
    addChildAndGet
    edit
    delete

insert :: SpecWith TestDB
insert =
  itDB "can insert (category)" $ do
    conn <- getConnection
    id <- lift $ runWithState conn $ addCategory "testName" Nothing
    category <- lift $ runWithState conn getRootCategoriesTree
    category `shouldBe` [testCategory]

getById :: SpecWith TestDB
getById =
  itDB "can get by id" $ do
    conn <- getConnection
    category <- lift $ runWithState conn $ getCategoryTreeFromBottom 1
    category `shouldBe` testCategory

addChildAndGet :: SpecWith TestDB
addChildAndGet =
  itDB "can add and get child" $ do
    conn <- getConnection
    id <- lift $ runWithState conn $ addCategory "testChildName" (Just 1)
    category <- lift $ runWithState conn getRootCategoriesTree
    category `shouldBe` [testParentCategory]

edit :: SpecWith TestDB
edit =
  itDB "edit" $ do
    conn <- getConnection
    lift $ runWithState conn $ editCategory 2 (Just "newTestName") (Just 0)
    categories <- lift $ runWithState conn getRootCategoriesTree
    categories `shouldBe` [testCategory, testEditedCategory]

delete :: SpecWith TestDB
delete =
  itDB "delete" $ do
    conn <- getConnection
    lift $ runWithState conn $ deleteCategory 1
    lift $ runWithState conn $ deleteCategory 2
    category <- lift $ runWithState conn getRootCategoriesTree
    category `shouldBe` []

testCategory :: Category
testCategory = Category 1 "testName" Nothing

testEditedCategory :: Category
testEditedCategory = Category 2 "newTestName" Nothing

testChildCategory :: Category
testChildCategory = Category 2 "testChildName" Nothing

testParentCategory :: Category
testParentCategory = testCategory {categoryChilds = Just [testChildCategory]}
