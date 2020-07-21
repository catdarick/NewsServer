{-# LANGUAGE OverloadedStrings #-}

module MethodsTest.Tag where

import           Api.Methods.Create.Account
import           Api.Methods.Create.Tag
import           Api.Methods.Delete.Tag
import           Api.Methods.Delete.User
import           Api.Methods.Get.Tag
import           Api.Methods.Get.Token
import           Api.Methods.Get.User
import           Api.Types.Tag
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
  describeDB initDatabase "Methods.Tag: " $ do
    User.createUser
    User.createAdmin
    createTagByUser
    createFirstTagByAdmin
    createSecondTagByAdmin
    createDuplicateTag
    createMissingName
    createMissingToken
    getTags_
    getById
    getByName
    deleteByUser
    deleteByAdmin
    getAfterDelete

createTagByUser :: SpecWith TestDB
createTagByUser =
  itDB "user can't create tag" $ do
    conn <- getConnection
    token <- User.getUserToken conn
    (status, resp) <- lift $ createTag conn (query token)
    (status, resp & responseSuccess) `shouldBe` (status404, False)
  where
    query token = [("name", Just "someName1"), ("token", Just token)]

createFirstTagByAdmin :: SpecWith TestDB
createFirstTagByAdmin =
  itDB "admin can create tag" $ do
    conn <- getConnection
    token <- User.getAdminToken conn
    (status, resp) <- lift $ createTag conn (query token)
    (status, resp & responseSuccess) `shouldBe` (status200, True)
  where
    query token = [("name", Just "someName1"), ("token", Just token)]

createSecondTagByAdmin :: SpecWith TestDB
createSecondTagByAdmin =
  itDB "admin can create one more tag" $ do
    conn <- getConnection
    token <- User.getAdminToken conn
    (status, resp) <- lift $ createTag conn (query token)
    (status, resp & responseSuccess) `shouldBe` (status200, True)
  where
    query token =
      [ ("name", Just "someName2")
      , ("token", Just token)
      ]

createDuplicateTag :: SpecWith TestDB
createDuplicateTag =
  itDB "admin can't create duplicate tag" $ do
    conn <- getConnection
    token <- User.getAdminToken conn
    (status, resp) <- lift $ createTag conn (query token)
    (status, resp & responseSuccess) `shouldBe` (status400, False)
  where
    query token =
      [ ("name", Just "someName2")
      , ("token", Just token)
      ]
createMissingName :: SpecWith TestDB
createMissingName =
  itDB "admin can't create tag without required 'name'" $ do
    conn <- getConnection
    token <- User.getAdminToken conn
    (status, resp) <- lift $ createTag conn (query token)
    (status, resp & responseSuccess) `shouldBe` (status404, False)
  where
    query token = [("token", Just token)]

createMissingToken :: SpecWith TestDB
createMissingToken =
  itDB "can't create tag without required 'token'" $ do
    conn <- getConnection
    (status, resp) <- lift $ createTag conn query 
    (status, resp & responseSuccess) `shouldBe` (status404, False)
  where
    query  = [("name", Just "someName3")]

getTags_ :: SpecWith TestDB
getTags_ =
  itDB "can get tag with child" $ do
    conn <- getConnection
    (status, resp) <- lift $ getTags conn query
    (status, resp & responseResult) `shouldBe`
      (status200, Just [testTag1, testTag2])
  where
    query = []

getById :: SpecWith TestDB
getById =
  itDB "can get tag by id" $ do
    conn <- getConnection
    (status, resp) <- lift $ getTags conn query
    (status, resp & responseResult) `shouldBe`
      (status200, Just [testTag1])
  where
    query = [("tag_id", Just "1")]

getByName :: SpecWith TestDB
getByName =
  itDB "can get tag by name" $ do
    conn <- getConnection
    (status, resp) <- lift $ getTags conn query
    (status, resp & responseResult) `shouldBe`
      (status200, Just [testTag1])
  where
    query = [("name", Just "someName1")]

deleteByUser :: SpecWith TestDB
deleteByUser =
  itDB "user can't delete tag" $ do
    conn <- getConnection
    token <- User.getUserToken conn
    (status, resp) <- lift $ deleteTag conn (query token)
    (status, resp & responseSuccess) `shouldBe` (status404, False)
  where
    query token = [("tag_id", Just "1"), ("token", Just token)]

deleteByAdmin :: SpecWith TestDB
deleteByAdmin =
  itDB "admin can delete tag" $ do
    conn <- getConnection
    token <- User.getAdminToken conn
    (status, resp) <- lift $ deleteTag conn (query token)
    (status, resp & responseSuccess) `shouldBe` (status200, True)
  where
    query token = [("tag_id", Just "1"), ("token", Just token)]

getAfterDelete :: SpecWith TestDB
getAfterDelete =
  itDB "is only 1 tag exists after delete" $ do
    conn <- getConnection
    (status, resp) <- lift $ getTags conn query
    (status, resp & responseResult) `shouldBe`
      (status200, Just [testTag2])
  where
    query = []

testTag1= Tag 1 "someName1"
testTag2 = Tag 2 "someName2"