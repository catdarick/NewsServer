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
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Transact   (getConnection)
import           Migration.Create
import           Test.Hspec                     (Spec, SpecWith, hspec)
import           Test.Hspec.DB
import           Test.Hspec.Expectations.Lifted

testCategory = Category 1 "testName" Nothing

testEditedCategory = Category 2 "newTestName" Nothing

testChildCategory = Category 2 "testChildName" Nothing

testParentCategory = testCategory {categoryChilds = Just [testChildCategory]}

spec :: Spec
spec =
  describeDB initDatabase "Category: " $ do
    insert
    getById
    addChildAndGet
    edit
    delete

insert :: SpecWith TestDB
insert =
  itDB "can insert (category)" $ do
    conn <- getConnection
    id <- lift $ addCategory conn "testName" Nothing
    category <- lift $ getRootCategoriesTree conn
    category `shouldBe` [testCategory]

getById :: SpecWith TestDB
getById =
  itDB "can get by id" $ do
    conn <- getConnection
    category <- lift $ getCategoryTreeFromBottom conn 1
    category `shouldBe` testCategory

addChildAndGet :: SpecWith TestDB
addChildAndGet =
  itDB "can add and get child" $ do
    conn <- getConnection
    id <- lift $ addCategory conn "testChildName" (Just 1)
    category <- lift $ getRootCategoriesTree conn
    category `shouldBe` [testParentCategory]

edit :: SpecWith TestDB
edit =
  itDB "edit" $ do
    conn <- getConnection
    lift $ editCategory conn 2 (Just "newTestName") (Just 0)
    categories <- lift $ getRootCategoriesTree conn
    categories `shouldBe` [testCategory, testEditedCategory]

delete :: SpecWith TestDB
delete =
  itDB "delete" $ do
    conn <- getConnection
    lift $ deleteCategory conn 1
    lift $ deleteCategory conn 2
    category <- lift $ getRootCategoriesTree conn
    category `shouldBe` []
