{-# LANGUAGE OverloadedStrings #-}

module MethodsTest.Comment where

import           Api.Methods.Create.Account
import           Api.Methods.Delete.Comment
import           Api.Methods.Delete.User
import           Api.Methods.Edit.Draft
import           Api.Methods.Get.Comment
import           Api.Methods.Post.Comment
import           Api.Methods.Post.Draft
import           Api.Types.Comment
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
import           Database.PostgreSQL.Transact   (DBT, getConnection)
import qualified MethodsTest.Author             as Author
import qualified MethodsTest.Category           as Category
import qualified MethodsTest.Draft              as Draft
import qualified MethodsTest.Tag                as Tag
import qualified MethodsTest.User               as User
import           Migration.Create
import           Network.HTTP.Types.Status      (Status, status200, status400,
                                                 status403, status404)
import           Test.Hspec                     (Spec, SpecWith, hspec)
import           Test.Hspec.DB
import           Test.Hspec.Expectations.Lifted

spec :: Spec
spec =
  describeDB initDatabase "Methods.News: " $ do
    User.createUserAdminAuthorAccounts
    Author.createAuthor1ByAdmin
    Tag.createFirstTagByAdmin
    Tag.createSecondTagByAdmin
    Category.createCategoryByAdmin
    Category.createChildCategoryByAdmin
    Draft.createDraftByAuthor1
    postCommentBeforePost
    Draft.postDraft1
    postCommentAfterPost
    getComment
    deleteCommentByAuthor
    deleteCommentByUser
    getCommentAfterDelete
    postCommentAfterPost
    deleteCommentByAdmin
    getCommentAfterDelete

postCommentByUser :: DBT IO (Status, Response Idcont)
postCommentByUser = do
  conn <- getConnection
  token <- User.getUserToken conn
  lift $ postComment conn (query token)
  where
    query token =
      [ ("news_id", Just "1")
      , ("content", Just "someContent1")
      , ("token", Just token)
      ]

postCommentBeforePost :: SpecWith TestDB
postCommentBeforePost =
  itDB "user can't post comment before draft is published" $ do
    (status, resp) <- postCommentByUser
    (status, resp & responseSuccess) `shouldBe` (status400, False)

postCommentAfterPost :: SpecWith TestDB
postCommentAfterPost =
  itDB "user can post comment after draft is published" $ do
    (status, resp) <- postCommentByUser
    (status, resp & responseSuccess) `shouldBe` (status200, True)

getComment :: SpecWith TestDB
getComment =
  itDB "can get comment" $ do
    conn <- getConnection
    (status, resp) <- lift $ getComments conn query
    (status, withDefTime_ (resp & responseResult)) `shouldBe`
      (status200, Just [testComment1])
  where
    query = [("news_id", Just "1")]

deleteCommentByAuthor :: SpecWith TestDB
deleteCommentByAuthor =
  itDB "non admin or creator account can't delete comment" $ do
    conn <- getConnection
    token <- User.getAuthor1Token conn
    (status, resp) <- lift $ deleteComment conn (query token)
    (status, resp & responseSuccess) `shouldBe` (status403, False)
  where
    query token = [("token", Just token), ("comment_id", Just "1")]

deleteCommentByUser :: SpecWith TestDB
deleteCommentByUser =
  itDB "user can delete own comment" $ do
    conn <- getConnection
    token <- User.getUserToken conn
    (status, resp) <- lift $ deleteComment conn (query token)
    (status, resp & responseSuccess) `shouldBe` (status200, True)
  where
    query token = [("token", Just token), ("comment_id", Just "1")]

getCommentAfterDelete :: SpecWith TestDB
getCommentAfterDelete =
  itDB "comment is deleted" $ do
    conn <- getConnection
    (status, resp) <- lift $ getComments conn query
    (status, resp & responseResult) `shouldBe`
      (status200, Just [])
  where
    query = [("news_id", Just "1")]
deleteCommentByAdmin :: SpecWith TestDB
deleteCommentByAdmin =
  itDB "admin can delete comment" $ do
    conn <- getConnection
    token <- User.getUserToken conn
    (status, resp) <- lift $ deleteComment conn (query token)
    (status, resp & responseSuccess) `shouldBe` (status200, True)
  where
    query token = [("token", Just token), ("comment_id", Just "2")]

withDefTime :: Comment -> Comment
withDefTime comment@Comment {commentUser = user} =
  comment {commentCreationTime = defTime, commentUser = User.withDefTime user}

withDefTime_ = (fmap . fmap) withDefTime

defTime = (LocalTime (ModifiedJulianDay 0) midnight)

testComment1 = Comment 1 User.testUser defTime "someContent1"

testComment2 = Comment 2 User.testUser defTime "someContent1"
