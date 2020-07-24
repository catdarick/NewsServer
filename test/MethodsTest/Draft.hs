{-# LANGUAGE OverloadedStrings #-}

module MethodsTest.Draft where

import           Api.ErrorException
import qualified Api.Errors                     as Err
import           Api.Methods.Create.Draft
import           Api.Methods.Delete.Draft
import           Api.Methods.Edit.Draft
import           Api.Methods.Get.Draft
import           Api.Methods.Get.Token
import           Api.Methods.Post.Draft
import           Api.Types.News
import           Api.Types.Response
import           Control.Exception              (try)
import           Control.Monad
import           Control.Monad.Trans.Class      (MonadTrans (lift))
import           Data.Function                  ((&))
import           Data.Maybe                     (fromJust)
import           Data.Time.Calendar             (Day (ModifiedJulianDay))
import           Data.Time.LocalTime            (LocalTime (LocalTime),
                                                 midnight)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Transact   (getConnection)
import qualified MethodsTest.Author             as Author
import qualified MethodsTest.Category           as Category
import           MethodsTest.Helper
import qualified MethodsTest.Tag                as Tag
import qualified MethodsTest.User               as User
import           Migration.Create
import           Network.HTTP.Types.Status      (status200, status400,
                                                 status403, status404)
import           Test.Hspec                     (Spec, SpecWith, hspec)
import           Test.Hspec.DB
import           Test.Hspec.Expectations.Lifted

spec :: Spec
spec =
  describeDB initDatabase "Methods.Draft: " $ do
    User.createUserAdminAuthorAccounts
    Author.createAuthor1ByAdmin
    Author.createAuthor2ByAdmin
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
    getDraftBy2Author
    editDraftBy1Author
    isDraftCorrectlyEdited
    editDraftBy2Author
    deleteDraftBy2Author
    deleteDraftBy1Author
    isDraftDeleted

createDraftByUser :: SpecWith TestDB
createDraftByUser =
  itDB "user can't create draft" $ do
    conn <- getConnection
    token <- User.getUserToken conn
    res <- lift $ try $ createDraft conn (query token)
    res `shouldBe` (Left $ ErrorException status403 Err.notAuthor)
  where
    query token =
      [ ("title", Just "someTitle1")
      , ("content", Just "someContent1")
      , ("category_id", Just "2")
      , ("tags_id", Just "[1]")
      , ("token", Just token)
      ]

createDraftByAuthor1 :: SpecWith TestDB
createDraftByAuthor1 =
  itDB "author1 can create draft" $ do
    conn <- getConnection
    token <- User.getAuthor1Token conn
    resp <- lift $ createDraft conn (query token)
    (resp & responseSuccess) `shouldBe` True
  where
    query token =
      [ ("title", Just "someTitle1")
      , ("content", Just "someContent1")
      , ("category_id", Just "2")
      , ("tags_id", Just "[1]")
      , ("token", Just token)
      ]

createDraftByAuthor2 :: SpecWith TestDB
createDraftByAuthor2 =
  itDB "author2 can create draft" $ do
    conn <- getConnection
    token <- User.getAuthor2Token conn
    resp <- lift $ createDraft conn (query token)
    (resp & responseSuccess) `shouldBe` True
  where
    query token =
      [ ("title", Just "someTitle2")
      , ("content", Just "someContent2")
      , ("category_id", Just "2")
      , ("tags_id", Just "[1,2]")
      , ("token", Just token)
      ]

createDraftMissingTitle :: SpecWith TestDB
createDraftMissingTitle =
  itDB "can't create draft without required 'title'" $ do
    conn <- getConnection
    token <- User.getAuthor1Token conn
    res <- lift $ try $ createDraft conn (query token)
    withEmptyError res `shouldBe` (Left $ ErrorException status400 "")
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
    res <- lift $ try $ createDraft conn (query token)
    withEmptyError res `shouldBe` (Left $ ErrorException status400 "")
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
    res <- lift $ try $ createDraft conn (query token)
    withEmptyError res `shouldBe` (Left $ ErrorException status400 "")
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
    res <- lift $ try $ createDraft conn (query token)
    withEmptyError res `shouldBe` (Left $ ErrorException status400 "")
  where
    query token =
      [ ("title", Just "someTitle2")
      , ("content", Just "someContent2")
      , ("tags_id", Just "[1,2]")
      , ("category_id", Just "1")
      ]

postDraft1 :: SpecWith TestDB
postDraft1 =
  itDB "author can post draft" $ do
    conn <- getConnection
    token <- User.getAuthor1Token conn
    resp <- lift $ postDraft conn (query token)
    (resp & responseSuccess) `shouldBe` True
  where
    query token = [("draft_id", Just "1"), ("token", Just token)]

postDraft2 :: SpecWith TestDB
postDraft2 =
  itDB "author can post draft" $ do
    conn <- getConnection
    token <- User.getAuthor2Token conn
    resp <- lift $ postDraft conn (query token)
    (resp & responseSuccess) `shouldBe` True
  where
    query token = [("draft_id", Just "2"), ("token", Just token)]

getDraftBy1Author :: SpecWith TestDB
getDraftBy1Author =
  itDB "can get 1st author's draft" $ do
    conn <- getConnection
    token <- User.getAuthor1Token conn
    resp <- lift $ getDrafts conn (query token)
    (withDefTime_ (resp & responseResult)) `shouldBe` Just [testDraft1]
  where
    query token = [("token", Just token)]

getDraftBy2Author :: SpecWith TestDB
getDraftBy2Author =
  itDB "can get 2'st author's draft" $ do
    conn <- getConnection
    token <- User.getAuthor2Token conn
    resp <- lift $ getDrafts conn (query token)
    (withDefTime_ (resp & responseResult)) `shouldBe` Just [testDraft2]
  where
    query token = [("token", Just token)]

editDraftBy1Author :: SpecWith TestDB
editDraftBy1Author =
  itDB "can edit 1st author's draft by himself" $ do
    conn <- getConnection
    token <- User.getAuthor1Token conn
    resp <- lift $ editDraft conn (query token)
    (resp & responseSuccess) `shouldBe` True
  where
    query token =
      [ ("draft_id", Just "1")
      , ("title", Just "someEditedTitle")
      , ("content", Just "someEditedContent")
      , ("category_id", Just "2")
      , ("tags_id", Just "[1,2]")
      , ("token", Just token)
      ]

isDraftCorrectlyEdited :: SpecWith TestDB
isDraftCorrectlyEdited =
  itDB "draft is correctly edited" $ do
    conn <- getConnection
    token <- User.getAuthor1Token conn
    resp <- lift $ getDrafts conn (query token)
    (withDefTime_ (resp & responseResult)) `shouldBe` Just [editedDraft1]
  where
    query token = [("token", Just token)]

editDraftBy2Author :: SpecWith TestDB
editDraftBy2Author =
  itDB "can't edit 1st author's draft by author2" $ do
    conn <- getConnection
    token <- User.getAuthor2Token conn
    res <- lift $ try $ editDraft conn (query token)
    res `shouldBe` (Left $ ErrorException status403 Err.noPerms)
  where
    query token =
      [ ("draft_id", Just "1")
      , ("title", Just "someEditedTitle")
      , ("content", Just "someEditedContent")
      , ("category_id", Just "2")
      , ("tags_id", Just "[1,2]")
      , ("token", Just token)
      ]

deleteDraftBy2Author :: SpecWith TestDB
deleteDraftBy2Author =
  itDB "author2 can't delete 1st author's draft" $ do
    conn <- getConnection
    token <- User.getAuthor2Token conn
    res <- lift $ try $ deleteDraft conn (query token)
    res `shouldBe` (Left $ ErrorException status403 Err.noPerms)
  where
    query token = [("draft_id", Just "1"), ("token", Just token)]

deleteDraftBy1Author :: SpecWith TestDB
deleteDraftBy1Author =
  itDB "author1 can delete 1st author's draft" $ do
    conn <- getConnection
    token <- User.getAuthor1Token conn
    resp <- lift $ deleteDraft conn (query token)
    (resp & responseSuccess) `shouldBe` True
  where
    query token = [("draft_id", Just "1"), ("token", Just token)]

isDraftDeleted :: SpecWith TestDB
isDraftDeleted =
  itDB "draft is deleted" $ do
    conn <- getConnection
    token <- User.getAuthor1Token conn
    resp <- lift $ getDrafts conn (query token)
    (resp & responseResult) `shouldBe` Just []
  where
    query token = [("token", Just token)]

withDefTime :: News -> News
withDefTime draft@News {newsAuthor = author} =
  draft {newsCreationTime = defTime, newsAuthor = Author.withDefTime <$> author}

withDefTime_ :: Maybe [News] -> Maybe [News]
withDefTime_ = (fmap . fmap) withDefTime

testDraft1 :: News
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

editedDraft1 :: News
editedDraft1 =
  News
    { newsId = 1
    , newsCategory = Category.testCategory
    , newsCreationTime = defTime
    , newsTitle = "someEditedTitle"
    , newsContent = "someEditedContent"
    , newsTags = [Tag.testTag1, Tag.testTag2]
    , newsMainPicture = Nothing
    , newsAdditionalPictures = Nothing
    , newsAuthor = Nothing
    }

testDraft2 :: News
testDraft2 =
  News
    { newsId = 2
    , newsCategory = Category.testCategory
    , newsCreationTime = defTime
    , newsTitle = "someTitle2"
    , newsContent = "someContent2"
    , newsTags = [Tag.testTag1, Tag.testTag2]
    , newsMainPicture = Nothing
    , newsAdditionalPictures = Nothing
    , newsAuthor = Nothing
    }
