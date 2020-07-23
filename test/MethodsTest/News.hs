{-# LANGUAGE OverloadedStrings #-}

module MethodsTest.News where

import           Api.Methods.Create.Account
import           Api.Methods.Delete.Draft
import           Api.Methods.Delete.User
import           Api.Methods.Edit.Draft
import           Api.Methods.Get.News
import           Api.Methods.Post.Draft
import           Api.Methods.Post.Comment
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
import qualified MethodsTest.Draft              as Draft
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
  describeDB initDatabase "Methods.News: " $ do
    User.createUserAdminAuthorAccounts
    Author.createAuthor1ByAdmin
    Author.createAuthor2ByAdmin
    Tag.createFirstTagByAdmin
    Tag.createSecondTagByAdmin
    Category.createCategoryByAdmin
    Category.createChildCategoryByAdmin
    Draft.createDraftByAuthor1
    Draft.createDraftByAuthor2
    Draft.postDraft1
    Draft.postDraft2
    getNewsByUser
    getByAuthorId
    getByLogin
    getByFName
    getByLName
    getNewsByCategoryId
    getNewsByTagId
    getNewsByTagsIdIn
    getNewsByTagsIdAll
    getNewsByTitleSearch1
    getNewsByTitleSearch2
    getNewsByContentSearch1
    getNewsByContentSearch2
    getNewsWithSort3
    getNewsWithSort4


getNewsByUser :: SpecWith TestDB
getNewsByUser =
  itDB "user can get news" $ do
    conn <- getConnection
    resp <- lift $ getNews conn query
    (fromJust $ withDefTime_ (resp & responseResult)) `shouldMatchList`
      [testNews1, testNews2]
  where
    query = []

getByAuthorId :: SpecWith TestDB
getByAuthorId =
  itDB "can get by author id" $ do
    conn <- getConnection
    resp <- lift $ getNews conn query
    (withDefTime_ (resp & responseResult)) `shouldBe`
      Just [testNews1]
  where
    query = [("author_id", Just "1")]


getByLogin :: SpecWith TestDB
getByLogin =
  itDB "can get by login" $ do
    conn <- getConnection
    resp <- lift $ getNews conn query
    (withDefTime_ (resp & responseResult)) `shouldBe`
      Just [testNews1]
  where
    query = [("login", Just "author1")]

getByFName :: SpecWith TestDB
getByFName =
  itDB "can get by first name" $ do
    conn <- getConnection
    resp <- lift $ getNews conn query
    (withDefTime_ (resp & responseResult)) `shouldBe`
      Just [testNews1]
  where
    query = [("first_name", Just "author1FName")]

getByLName :: SpecWith TestDB
getByLName =
  itDB "can get by first name" $ do
    conn <- getConnection
    resp <- lift $ getNews conn query
    (withDefTime_ (resp & responseResult)) `shouldBe`
      Just [testNews1]
  where
    query = [("last_name", Just "author1LName")]

getNewsByCategoryId :: SpecWith TestDB
getNewsByCategoryId =
  itDB "can get by category id" $ do
    conn <- getConnection
    resp <- lift $ getNews conn query
    (fromJust $ withDefTime_ (resp & responseResult)) `shouldMatchList`
      [testNews1, testNews2]
  where
    query = [("category_id", Just "2")]
getNewsByTagId :: SpecWith TestDB
getNewsByTagId =
  itDB "can get by tag id name" $ do
    conn <- getConnection
    resp <- lift $ getNews conn query
    (withDefTime_ (resp & responseResult)) `shouldBe`
      Just [testNews2]
  where
    query = [("tag_id", Just "2")]
getNewsByTagsIdIn:: SpecWith TestDB
getNewsByTagsIdIn =
  itDB "can get by tags_in" $ do
    conn <- getConnection
    resp <- lift $ getNews conn query
    (fromJust $ withDefTime_ (resp & responseResult)) `shouldMatchList`
      [testNews1, testNews2]
  where
    query = [("tags_id_in", Just "[1,2,3]")]
getNewsByTagsIdAll :: SpecWith TestDB
getNewsByTagsIdAll =
  itDB "can get by tags_all" $ do
    conn <- getConnection
    resp <- lift $ getNews conn query
    (withDefTime_ (resp & responseResult)) `shouldBe`
      Just [testNews2]
  where
    query = [("tags_id_all", Just "[1,2]")]
getNewsByTitleSearch1 :: SpecWith TestDB
getNewsByTitleSearch1 =
  itDB "get both news by title search" $ do
    conn <- getConnection
    resp <- lift $ getNews conn query
    (fromJust $ withDefTime_ (resp & responseResult)) `shouldMatchList`
      [testNews1, testNews2]
  where
    query = [("title", Just "some")]
getNewsByTitleSearch2 :: SpecWith TestDB
getNewsByTitleSearch2 =
  itDB "get only one news by title search" $ do
    conn <- getConnection
    resp <- lift $ getNews conn query
    (withDefTime_ (resp & responseResult)) `shouldBe`
      Just [testNews2]
  where
    query = [("title", Just "Title2")]
getNewsByContentSearch1 :: SpecWith TestDB
getNewsByContentSearch1 =
  itDB "get both news by content search" $ do
    conn <- getConnection
    resp <- lift $ getNews conn query
    (fromJust $ withDefTime_ (resp & responseResult)) `shouldMatchList`
      [testNews1, testNews2]
  where
    query = [("content", Just "some")]
getNewsByContentSearch2 :: SpecWith TestDB
getNewsByContentSearch2 =
  itDB "get only one news by content search" $ do
    conn <- getConnection
    resp <- lift $ getNews conn query
    (withDefTime_ (resp & responseResult)) `shouldBe`
      Just [testNews2]
  where
    query = [("content", Just "Content2")]

getNewsWithSort3:: SpecWith TestDB
getNewsWithSort3 =
  itDB "correctly sorted by author name DESC" $ do
    conn <- getConnection
    resp <- lift $ getNews conn query
    (withDefTime_ (resp & responseResult)) `shouldBe`
      Just [testNews1, testNews2]
  where
    query = [("sort", Just "3")]
getNewsWithSort4:: SpecWith TestDB
getNewsWithSort4 =
  itDB "correctly sorted by author name ASC" $ do
    conn <- getConnection
    resp <- lift $ getNews conn query
    (withDefTime_ (resp & responseResult)) `shouldBe`
      Just [testNews2, testNews1]
  where
    query = [("sort", Just "4")]

defTime = (LocalTime (ModifiedJulianDay 0) midnight)

withDefTime draft@News {newsAuthor = author} =
  draft {newsCreationTime = defTime, newsAuthor = Author.withDefTime <$> author}

withDefTime_ = (fmap . fmap) withDefTime

testNews1 =
  News
    { newsId = 1
    , newsCategory = Category.testCategory
    , newsCreationTime = defTime
    , newsTitle = "someTitle1"
    , newsContent = "someContent1"
    , newsTags = [Tag.testTag1]
    , newsMainPicture = Nothing
    , newsAdditionalPictures = Nothing
    , newsAuthor = Just Author.testAuthor1
    }

testNews2 =
  News
    { newsId = 2
    , newsCategory = Category.testCategory
    , newsCreationTime = defTime
    , newsTitle = "someTitle2"
    , newsContent = "someContent2"
    , newsTags = [Tag.testTag1, Tag.testTag2]
    , newsMainPicture = Nothing
    , newsAdditionalPictures = Nothing
    , newsAuthor = Just Author.testAuthor2
    }
