{-# LANGUAGE OverloadedStrings #-}

module MethodsTest.Draft where

import           Api.Methods.Create.Account
import           Api.Methods.Create.Draft
import           Api.Methods.Delete.Draft
import           Api.Methods.Delete.User
import           Api.Methods.Get.Draft
import           Api.Methods.Get.Token
import           Api.Methods.Get.User
import           Api.Types.News
import           Api.Types.Response
import           Api.Types.Tag
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
import qualified MethodsTest.Author             as Author
import qualified MethodsTest.Category           as Category
import qualified MethodsTest.Tag                as Tag
import qualified MethodsTest.User               as User
import           Migration.Create
import           Network.HTTP.Types.Status      (status200, status400,
                                                 status404)
import           Test.Hspec                     (Spec, SpecWith, hspec)
import           Test.Hspec.DB
import           Test.Hspec.Expectations.Lifted

spec :: Spec
spec =
  describeDB initDatabase "Methods.Draft: " $ do
    User.createUserAdminAuthorAccounts
    Author.createAuthorByAdmin
    Author.createAuthorByAdmin2
    Tag.createFirstTagByAdmin
    Tag.createSecondTagByAdmin
    Category.createCategoryByAdmin
    Category.createChildCategoryByAdmin
    createDraftByUser
    createDraftByAuthor1
    createDraftByAuthor2
    createDraftMissingTitle
    createDraftMissingContent
    createDraftMissingCategory
    createDraftMissingToken
    getDraftBy1Author
    -- createFirstTagByAdmin
    -- createSecondTagByAdmin
    -- createDuplicateTag
    -- createMissingName
    -- createMissingToken
    -- getTags_
    -- getById
    -- getByName
    -- deleteByUser
    -- deleteByAdmin
    -- getAfterDelete

createDraftByUser :: SpecWith TestDB
createDraftByUser =
  itDB "user can't create draft" $ do
    conn <- getConnection
    token <- User.getUserToken conn
    (status, resp) <- lift $ createDraft conn (query token)
    (status, resp & responseSuccess) `shouldBe` (status400, False)
  where
    query token =
      [ ("title", Just "someTitle1")
      , ("content", Just "someContent1")
      , ("category_id", Just "1")
      , ("tags_id", Just "[1]")
      , ("token", Just token)
      ]

createDraftByAuthor1 :: SpecWith TestDB
createDraftByAuthor1 =
  itDB "author1 can create draft" $ do
    conn <- getConnection
    token <- User.getAuthor1Token conn
    (status, resp) <- lift $ createDraft conn (query token)
    (status, resp & responseSuccess) `shouldBe` (status200, True)
  where
    query token =
      [ ("title", Just "someTitle1")
      , ("content", Just "someContent1")
      , ("category_id", Just "1")
      , ("tags_id", Just "[1]")
      , ("token", Just token)
      ]

createDraftByAuthor2 :: SpecWith TestDB
createDraftByAuthor2 =
  itDB "author2 can create draft" $ do
    conn <- getConnection
    token <- User.getAuthor2Token conn
    (status, resp) <- lift $ createDraft conn (query token)
    (status, resp & responseSuccess) `shouldBe` (status200, True)
  where
    query token =
      [ ("title", Just "someTitle2")
      , ("content", Just "someContent2")
      , ("category_id", Just "1")
      , ("tags_id", Just "[1,2]")
      , ("token", Just token)
      ]

createDraftMissingTitle :: SpecWith TestDB
createDraftMissingTitle =
  itDB "can't create draft without required 'title'" $ do
    conn <- getConnection
    token <- User.getAuthor1Token conn
    (status, resp) <- lift $ createDraft conn (query token)
    (status, resp & responseSuccess) `shouldBe` (status400, False)
  where
    query token =
      [ ("content", Just "someContent2")
      , ("category_id", Just "1")
      , ("tags_id", Just "[1,2]")
      , ("token", Just token)
      ]

createDraftMissingContent :: SpecWith TestDB
createDraftMissingContent =
  itDB "can't create draft without required 'content'" $ do
    conn <- getConnection
    token <- User.getAuthor1Token conn
    (status, resp) <- lift $ createDraft conn (query token)
    (status, resp & responseSuccess) `shouldBe` (status400, False)
  where
    query token =
      [ ("title", Just "someTitle2")
      , ("category_id", Just "1")
      , ("tags_id", Just "[1,2]")
      , ("token", Just token)
      ]

createDraftMissingCategory :: SpecWith TestDB
createDraftMissingCategory =
  itDB "can't create draft without required 'category_id'" $ do
    conn <- getConnection
    token <- User.getAuthor1Token conn
    (status, resp) <- lift $ createDraft conn (query token)
    (status, resp & responseSuccess) `shouldBe` (status400, False)
  where
    query token =
      [ ("title", Just "someTitle2")
      , ("content", Just "someContent2")
      , ("tags_id", Just "[1,2]")
      , ("token", Just token)
      ]

createDraftMissingToken :: SpecWith TestDB
createDraftMissingToken =
  itDB "can't create draft without required 'token'" $ do
    conn <- getConnection
    token <- User.getAuthor1Token conn
    (status, resp) <- lift $ createDraft conn (query token)
    (status, resp & responseSuccess) `shouldBe` (status400, False)
  where
    query token =
      [ ("title", Just "someTitle2")
      , ("content", Just "someContent2")
      , ("tags_id", Just "[1,2]")
      , ("category_id", Just "1")
      ]

getDraftBy1Author :: SpecWith TestDB
getDraftBy1Author =
  itDB "can't create draft without required 'token'" $ do
    conn <- getConnection
    token <- User.getAuthor1Token conn
    (status, resp) <- lift $ getDrafts conn (query token)
    (status, withDefTime_ (resp & responseResult)) `shouldBe` (status200, Just [testDraft1])
  where
    query token =
      [ ("token", Just token)
      ]

-- createFirstTagByAdmin :: SpecWith TestDB
-- createFirstTagByAdmin =
--   itDB "admin can create tag" $ do
--     conn <- getConnection
--     token <- User.getAdminToken conn
--     (status, resp) <- lift $ createTag conn (query token)
--     (status, resp & responseSuccess) `shouldBe` (status200, True)
--   where
--     query token = [("name", Just "someName1"), ("token", Just token)]
-- createSecondTagByAdmin :: SpecWith TestDB
-- createSecondTagByAdmin =
--   itDB "admin can create one more tag" $ do
--     conn <- getConnection
--     token <- User.getAdminToken conn
--     (status, resp) <- lift $ createTag conn (query token)
--     (status, resp & responseSuccess) `shouldBe` (status200, True)
--   where
--     query token = [("name", Just "someName2"), ("token", Just token)]
-- createDuplicateTag :: SpecWith TestDB
-- createDuplicateTag =
--   itDB "admin can't create duplicate tag" $ do
--     conn <- getConnection
--     token <- User.getAdminToken conn
--     (status, resp) <- lift $ createTag conn (query token)
--     (status, resp & responseSuccess) `shouldBe` (status400, False)
--   where
--     query token = [("name", Just "someName2"), ("token", Just token)]
-- createMissingName :: SpecWith TestDB
-- createMissingName =
--   itDB "admin can't create tag without required 'name'" $ do
--     conn <- getConnection
--     token <- User.getAdminToken conn
--     (status, resp) <- lift $ createTag conn (query token)
--     (status, resp & responseSuccess) `shouldBe` (status404, False)
--   where
--     query token = [("token", Just token)]
-- createMissingToken :: SpecWith TestDB
-- createMissingToken =
--   itDB "can't create tag without required 'token'" $ do
--     conn <- getConnection
--     (status, resp) <- lift $ createTag conn query
--     (status, resp & responseSuccess) `shouldBe` (status404, False)
--   where
--     query = [("name", Just "someName3")]
-- getTags_ :: SpecWith TestDB
-- getTags_ =
--   itDB "can get tag with child" $ do
--     conn <- getConnection
--     (status, resp) <- lift $ getTags conn query
--     (status, resp & responseResult) `shouldBe`
--       (status200, Just [testTag1, testTag2])
--   where
--     query = []
-- getById :: SpecWith TestDB
-- getById =
--   itDB "can get tag by id" $ do
--     conn <- getConnection
--     (status, resp) <- lift $ getTags conn query
--     (status, resp & responseResult) `shouldBe` (status200, Just [testTag1])
--   where
--     query = [("tag_id", Just "1")]
-- getByName :: SpecWith TestDB
-- getByName =
--   itDB "can get tag by name" $ do
--     conn <- getConnection
--     (status, resp) <- lift $ getTags conn query
--     (status, resp & responseResult) `shouldBe` (status200, Just [testTag1])
--   where
--     query = [("name", Just "someName1")]
-- deleteByUser :: SpecWith TestDB
-- deleteByUser =
--   itDB "user can't delete tag" $ do
--     conn <- getConnection
--     token <- User.getUserToken conn
--     (status, resp) <- lift $ deleteTag conn (query token)
--     (status, resp & responseSuccess) `shouldBe` (status404, False)
--   where
--     query token = [("tag_id", Just "1"), ("token", Just token)]
-- deleteByAdmin :: SpecWith TestDB
-- deleteByAdmin =
--   itDB "admin can delete tag" $ do
--     conn <- getConnection
--     token <- User.getAdminToken conn
--     (status, resp) <- lift $ deleteTag conn (query token)
--     (status, resp & responseSuccess) `shouldBe` (status200, True)
--   where
--     query token = [("tag_id", Just "1"), ("token", Just token)]
-- getAfterDelete :: SpecWith TestDB
-- getAfterDelete =
--   itDB "is only 1 tag exists after delete" $ do
--     conn <- getConnection
--     (status, resp) <- lift $ getTags conn query
--     (status, resp & responseResult) `shouldBe` (status200, Just [testTag2])
--   where
--     query = []
-- testTag1 = Tag 1 "someName1"
-- testTag2 = Tag 2 "someName2"
defTime = (LocalTime (ModifiedJulianDay 0) midnight)

withDefTime draft@News {newsAuthor = author} =
  draft {newsCreationTime = defTime, newsAuthor = Author.withDefTime <$> author}
withDefTime_ = (fmap . fmap) withDefTime
testDraft1 =
  News
    { newsId = 1
    , newsCategory = Category.testCategory
    , newsCreationTime = defTime
    , newsTitle = "someTitle1"
    , newsContent = "someContent1"
    , newsTags = [Tag.testTag1]
    , newsMainPicture = Nothing
    , newsAdditionalPictures = Nothing
    , newsAuthor = Nothing
    }
