{-# LANGUAGE OverloadedStrings #-}

module DatabaseTest.Draft where

import           Api.Types.News
import           Api.Types.Synonyms
import           Control.Monad.Trans.Class      (MonadTrans (lift))
import           Data.Function                  ((&))
import           Data.Time                      (Day (ModifiedJulianDay),
                                                 LocalTime (LocalTime),
                                                 midnight)
import           Database.Create.Draft
import           Database.Create.User
import           Database.Delete.Draft
import           Database.Edit.Draft
import           Database.Get.Draft
import qualified Database.Init                  as DB
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Transact   (getConnection)
import qualified DatabaseTest.Author            as Author
import qualified DatabaseTest.Category          as Category
import qualified DatabaseTest.Tag               as Tag
import           Test.Hspec                     (Spec, SpecWith, hspec)
import           Test.Hspec.DB
import           Test.Hspec.Expectations.Lifted
import           TestHelper

spec :: Spec
spec =
  describeDB DB.init "Draft: " $ do
    Author.insert
    Category.insert
    Tag.insert
    insert
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
    edit
    post

insert :: SpecWith TestDB
insert =
  itDB "can insert (draft)" $ do
    conn <- getConnection
    addTestDraft conn
    lift $ runWithState conn $ setToken 1 "token"
    draft <- getAllDrafts conn Nothing Nothing Nothing Nothing Nothing Nothing
    (withDefTime <$> draft) `shouldBe` [testDraft]
  where
    addTestDraft conn =
      lift $
      runWithState conn $
      addDraftWithTags 1 "testTitle" "testContent" 1 Nothing Nothing (Just [1])

getById :: SpecWith TestDB
getById =
  itDB "can get by category id" $ do
    conn <- getConnection
    draft <- getAllDrafts conn (Just 1) Nothing Nothing Nothing Nothing Nothing
    (withDefTime <$> draft) `shouldBe` [testDraft]

getByBadId :: SpecWith TestDB
getByBadId =
  itDB "get empty with incorrect category id" $ do
    conn <- getConnection
    draft <- getAllDrafts conn (Just 2) Nothing Nothing Nothing Nothing Nothing
    (withDefTime <$> draft) `shouldBe` []

getByTagId :: SpecWith TestDB
getByTagId =
  itDB "can get by categotagry id" $ do
    conn <- getConnection
    draft <- getAllDrafts conn Nothing (Just 1) Nothing Nothing Nothing Nothing
    (withDefTime <$> draft) `shouldBe` [testDraft]

getByBadTagId :: SpecWith TestDB
getByBadTagId =
  itDB "get empty with incorrect tag id" $ do
    conn <- getConnection
    draft <- getAllDrafts conn Nothing (Just 2) Nothing Nothing Nothing Nothing
    (withDefTime <$> draft) `shouldBe` []

getByTagsIn :: SpecWith TestDB
getByTagsIn =
  itDB "get by tags in" $ do
    conn <- getConnection
    draft <-
      getAllDrafts conn Nothing Nothing (Just [1, 2]) Nothing Nothing Nothing
    (withDefTime <$> draft) `shouldBe` [testDraft]

getByBadTagsIn :: SpecWith TestDB
getByBadTagsIn =
  itDB "get empty by bad tags in" $ do
    conn <- getConnection
    draft <-
      getAllDrafts conn Nothing Nothing (Just [2, 3]) Nothing Nothing Nothing
    (withDefTime <$> draft) `shouldBe` []

getByTagsAll :: SpecWith TestDB
getByTagsAll =
  itDB "get by tags all" $ do
    conn <- getConnection
    draft <-
      getAllDrafts conn Nothing Nothing Nothing (Just [1]) Nothing Nothing
    (withDefTime <$> draft) `shouldBe` [testDraft]

getByBadTagsAll :: SpecWith TestDB
getByBadTagsAll =
  itDB "get empty by bad tags all" $ do
    conn <- getConnection
    draft <-
      getAllDrafts conn Nothing Nothing Nothing (Just [1, 2]) Nothing Nothing
    (withDefTime <$> draft) `shouldBe` []

getByTitleSearch :: SpecWith TestDB
getByTitleSearch =
  itDB "get by search title" $ do
    conn <- getConnection
    draft <-
      getAllDrafts conn Nothing Nothing Nothing Nothing (Just "Title") Nothing
    (withDefTime <$> draft) `shouldBe` [testDraft]

getByBadTitleSearch :: SpecWith TestDB
getByBadTitleSearch =
  itDB "get empty by search bad title" $ do
    conn <- getConnection
    draft <-
      getAllDrafts
        conn
        Nothing
        Nothing
        Nothing
        Nothing
        (Just "badTitle")
        Nothing
    (withDefTime <$> draft) `shouldBe` []

getByContentSearch :: SpecWith TestDB
getByContentSearch =
  itDB "get by search title" $ do
    conn <- getConnection
    draft <-
      getAllDrafts conn Nothing Nothing Nothing Nothing Nothing (Just "Content")
    (withDefTime <$> draft) `shouldBe` [testDraft]

getByBadContentSearch :: SpecWith TestDB
getByBadContentSearch =
  itDB "get empty by search bad title" $ do
    conn <- getConnection
    draft <-
      getAllDrafts
        conn
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        (Just "badContent")
    (withDefTime <$> draft) `shouldBe` []

post :: SpecWith TestDB
post =
  itDB "post" $ do
    conn <- getConnection
    res <- lift $ runWithState conn $ publishDraft 1
    res `shouldBe` ()

edit :: SpecWith TestDB
edit =
  itDB "edit" $ do
    conn <- getConnection
    editDraft_ conn
    draft <- getAllDrafts conn Nothing Nothing Nothing Nothing Nothing Nothing
    (withDefTime <$> draft) `shouldBe` [editedDraft]
  where
    editDraft_ conn =
      lift $
      runWithState conn $
      editDraft
        1
        (Just "newTitle")
        (Just "newContent")
        Nothing
        Nothing
        Nothing
        Nothing

getAllDrafts ::
     MonadTrans t
  => Connection
  -> Maybe CategoryId
  -> Maybe TagId
  -> Maybe [TagId]
  -> Maybe [TagId]
  -> Maybe Title
  -> Maybe Content
  -> t IO [News]
getAllDrafts conn a b c d e f =
  lift $
  runWithState conn $ getDrafts "token" Nothing a b c d e f Nothing Nothing

withDefTime :: News -> News
withDefTime draft = draft {newsCreationTime = defTime}

testDraft :: News
testDraft =
  News
    1
    "testTitle"
    defTime
    Nothing
    [Tag.testTag]
    "testContent"
    Nothing
    Nothing
    Category.testCategory

editedDraft :: News
editedDraft =
  News
    1
    "newTitle"
    defTime
    Nothing
    [Tag.testTag]
    "newContent"
    Nothing
    Nothing
    Category.testCategory
