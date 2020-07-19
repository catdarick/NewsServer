{-# LANGUAGE OverloadedStrings #-}

module DatabaseTest.News where

import           Api.Types.Author
import           Api.Types.News
import           Api.Types.User
import           Control.Monad
import           Control.Monad.Trans.Class      (MonadTrans (lift))
import           Data.Function                  ((&))
import           Data.Time                      (Day (ModifiedJulianDay),
                                                 LocalTime (LocalTime),
                                                 midnight)
import           Database.Create.Author
import           Database.Create.Category
import           Database.Create.Draft
import           Database.Create.Tag
import           Database.Create.User
import           Database.Delete.Draft
import           Database.Edit.Draft
import           Database.Get.Author
import           Database.Get.Draft
import           Database.Get.News
import           Database.Get.User
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Transact   (getConnection)
import qualified DatabaseTest.Author            as Author
import qualified DatabaseTest.Category          as Category
import qualified DatabaseTest.Draft             as Draft
import qualified DatabaseTest.Tag               as Tag
import           Migration.Create
import           Test.Hspec                     (Spec, SpecWith, hspec)
import           Test.Hspec.DB
import           Test.Hspec.Expectations.Lifted

spec :: Spec
spec =
  describeDB initDatabase "News: " $ do
    Author.insert
    Category.insert
    Tag.insert
    Draft.insert
    Draft.post
    getById
    getByBadId
    getByTagId
    getByBadTagId
    getByTagsIn
    getByBadTagsIn
    getByTagsAll
    getByBadTagsAll
    getByTitleSearch
    getByBadTitleSearch
    getByContentSearch
    getByBadContentSearch

getById :: SpecWith TestDB
getById =
  itDB "can get by category id" $ do
    conn <- getConnection
    news <- getNews1 conn (Just 1) Nothing Nothing Nothing Nothing Nothing
    (withDefTime <$> news) `shouldBe` [testNews]

getByBadId :: SpecWith TestDB
getByBadId =
  itDB "get empty with incorrect category id" $ do
    conn <- getConnection
    news <- getNews1 conn (Just 2) Nothing Nothing Nothing Nothing Nothing
    (withDefTime <$> news) `shouldBe` []

getByTagId :: SpecWith TestDB
getByTagId =
  itDB "can get by categotagry id" $ do
    conn <- getConnection
    news <- getNews1 conn Nothing (Just 1) Nothing Nothing Nothing Nothing
    (withDefTime <$> news) `shouldBe` [testNews]

getByBadTagId :: SpecWith TestDB
getByBadTagId =
  itDB "get empty with incorrect tag id" $ do
    conn <- getConnection
    news <- getNews1 conn Nothing (Just 2) Nothing Nothing Nothing Nothing
    (withDefTime <$> news) `shouldBe` []

getByTagsIn :: SpecWith TestDB
getByTagsIn =
  itDB "get by tags in" $ do
    conn <- getConnection
    news <- getNews1 conn Nothing Nothing (Just [1, 2]) Nothing Nothing Nothing
    (withDefTime <$> news) `shouldBe` [testNews]

getByBadTagsIn :: SpecWith TestDB
getByBadTagsIn =
  itDB "get empty by bad tags in" $ do
    conn <- getConnection
    news <- getNews1 conn Nothing Nothing (Just [2, 3]) Nothing Nothing Nothing
    (withDefTime <$> news) `shouldBe` []

getByTagsAll :: SpecWith TestDB
getByTagsAll =
  itDB "get by tags all" $ do
    conn <- getConnection
    news <- getNews1 conn Nothing Nothing Nothing (Just [1]) Nothing Nothing
    (withDefTime <$> news) `shouldBe` [testNews]

getByBadTagsAll :: SpecWith TestDB
getByBadTagsAll =
  itDB "get empty by bad tags all" $ do
    conn <- getConnection
    news <- getNews1 conn Nothing Nothing Nothing (Just [1, 2]) Nothing Nothing
    (withDefTime <$> news) `shouldBe` []

getByTitleSearch :: SpecWith TestDB
getByTitleSearch =
  itDB "get by search title" $ do
    conn <- getConnection
    news <- getNews1 conn Nothing Nothing Nothing Nothing (Just "Title") Nothing
    (withDefTime <$> news) `shouldBe` [testNews]

getByBadTitleSearch :: SpecWith TestDB
getByBadTitleSearch =
  itDB "get empty by search bad title" $ do
    conn <- getConnection
    news <-
      getNews1 conn Nothing Nothing Nothing Nothing (Just "badTitle") Nothing
    (withDefTime <$> news) `shouldBe` []

getByContentSearch :: SpecWith TestDB
getByContentSearch =
  itDB "get by search title" $ do
    conn <- getConnection
    news <-
      getNews1 conn Nothing Nothing Nothing Nothing Nothing (Just "Content")
    (withDefTime <$> news) `shouldBe` [testNews]

getByBadContentSearch :: SpecWith TestDB
getByBadContentSearch =
  itDB "get empty by search bad title" $ do
    conn <- getConnection
    news <-
      getNews1 conn Nothing Nothing Nothing Nothing Nothing (Just "badContent")
    (withDefTime <$> news) `shouldBe` []

getNews1 conn a b c d e f =
  lift $
  getNews
    conn
    Nothing
    Nothing
    Nothing
    Nothing
    a
    b
    c
    d
    e
    f
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing

defTime = LocalTime (ModifiedJulianDay 0) midnight

withDefTime news@News {newsAuthor = author} =
  news {newsCreationTime = defTime, newsAuthor = Author.withDefTime <$> author}

testNews =
  News
    1
    "testTitle"
    defTime
    (Just Author.testAuthor)
    [Tag.testTag]
    "testContent"
    Nothing
    Nothing
    Category.testCategory

