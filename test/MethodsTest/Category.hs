{-# LANGUAGE OverloadedStrings #-}

module MethodsTest.Category where

import           Api.ErrorException
import qualified Api.Errors                     as Err
import           Api.Methods.Create.Category
import           Api.Methods.Delete.Category
import           Api.Methods.Get.Category
import           Api.Types.Category
import           Api.Types.Response
import           Control.Exception              (try)
import           Control.Monad.Trans.Class      (MonadTrans (lift))
import           Data.Function                  ((&))
import           Data.Maybe                     (fromJust)
import           Data.Time.Calendar             (Day (ModifiedJulianDay))
import           Data.Time.LocalTime            (LocalTime (LocalTime),
                                                 midnight)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Transact   (getConnection)
import           TestHelper
import qualified MethodsTest.User               as User
import qualified Database.Init                  as DB
import           Network.HTTP.Types.Status      (status200, status400,
                                                 status404)
import           Test.Hspec                     (Spec, SpecWith, hspec)
import           Test.Hspec.DB
import           Test.Hspec.Expectations.Lifted

spec :: Spec
spec =
  describeDB DB.init "Methods.Category: " $ do
    User.createUser
    User.createAdmin
    createCategoryByUser
    createCategoryByAdmin
    createChildCategoryByAdmin
    createMissingName
    createMissingToken
    getCategories_
    getById
    getChildByName
    getChildById
    deleteByUser
    deleteByAdmin
    getAfterDelete

createCategoryByUser :: SpecWith TestDB
createCategoryByUser =
  itDB "user can't create category" $ do
    conn <- getConnection
    token <- User.getUserToken conn
    res <- lift $ try $ createCategory conn (query token)
    res `shouldBe` (Left $ ErrorException status404 "")
  where
    query token = [("name", Just "someName"), ("token", Just token)]

createCategoryByAdmin :: SpecWith TestDB
createCategoryByAdmin =
  itDB "admin can create category" $ do
    conn <- getConnection
    token <- User.getAdminToken conn
    (status, resp) <- lift $ createCategory conn (query token)
    (resp & responseSuccess) `shouldBe` True
  where
    query token = [("name", Just "someName"), ("token", Just token)]

createChildCategoryByAdmin :: SpecWith TestDB
createChildCategoryByAdmin =
  itDB "admin can create child category" $ do
    conn <- getConnection
    token <- User.getAdminToken conn
    (status, resp) <- lift $ createCategory conn (query token)
    (resp & responseSuccess) `shouldBe` True
  where
    query token =
      [ ("name", Just "someName2")
      , ("token", Just token)
      , ("parent_id", Just "1")
      ]

createMissingName :: SpecWith TestDB
createMissingName =
  itDB "admin can't create category without required 'name'" $ do
    conn <- getConnection
    token <- User.getAdminToken conn
    res <- lift $ try $ createCategory conn (query token)
    withEmptyError res `shouldBe` (Left $ ErrorException status404 "")
  where
    query token = [("token", Just token)]

createMissingToken :: SpecWith TestDB
createMissingToken =
  itDB "can't create category without required 'token'" $ do
    conn <- getConnection
    res <- lift $ try $ createCategory conn query
    withEmptyError res `shouldBe` (Left $ ErrorException status404 "")
  where
    query = [("name", Just "someName")]

getCategories_ :: SpecWith TestDB
getCategories_ =
  itDB "can get category with child" $ do
    conn <- getConnection
    (status, resp) <- lift $ getCategories conn query
    (resp & responseResult) `shouldBe` Just [testCategory]
  where
    query = []

getById :: SpecWith TestDB
getById =
  itDB "can get category by id" $ do
    conn <- getConnection
    (status, resp) <- lift $ getCategories conn query
    (resp & responseResult) `shouldBe` Just [testCategory]
  where
    query = [("category_id", Just "1")]

getChildById :: SpecWith TestDB
getChildById =
  itDB "can get child category" $ do
    conn <- getConnection
    (status, resp) <- lift $ getCategories conn query
    (resp & responseResult) `shouldBe` Just [testChildCategory]
  where
    query = [("category_id", Just "2")]

getChildByName :: SpecWith TestDB
getChildByName =
  itDB "can get child by name" $ do
    conn <- getConnection
    (status, resp) <- lift $ getCategories conn query
    (resp & responseResult) `shouldBe` Just [testChildCategory]
  where
    query = [("name", Just "someName2")]

getChildByParentId :: SpecWith TestDB
getChildByParentId =
  itDB "can get child by parent_id" $ do
    conn <- getConnection
    (status, resp) <- lift $ getCategories conn query
    (resp & responseResult) `shouldBe` Just [testChildCategory]
  where
    query = [("parent_id", Just "1")]

deleteByUser :: SpecWith TestDB
deleteByUser =
  itDB "user can't delete category" $ do
    conn <- getConnection
    token <- User.getUserToken conn
    res <- lift $ try $ deleteCategory conn (query token)
    res `shouldBe` (Left $ ErrorException status404 "")
  where
    query token = [("category_id", Just "1"), ("token", Just token)]

deleteByAdmin :: SpecWith TestDB
deleteByAdmin =
  itDB "admin can delete category" $ do
    conn <- getConnection
    token <- User.getAdminToken conn
    (status, resp) <- lift $ deleteCategory conn (query token)
    (resp & responseSuccess) `shouldBe` True
  where
    query token = [("category_id", Just "1"), ("token", Just token)]

getAfterDelete :: SpecWith TestDB
getAfterDelete =
  itDB "result list is empty after delete parent category" $ do
    conn <- getConnection
    (status, resp) <- lift $ getCategories conn query
    (resp & responseResult) `shouldBe` Just []
  where
    query = []

testCategory :: Category
testCategory = Category 1 "someName" (Just [testChildCategory])

testChildCategory :: Category
testChildCategory = Category 2 "someName2" Nothing
