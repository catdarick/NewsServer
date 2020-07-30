{-# LANGUAGE OverloadedStrings #-}

module MethodsTest.Comment where

import           Api.ErrorException
import qualified Api.Errors                     as Err
import           Api.Methods.Create.Comment
import           Api.Methods.Delete.Comment
import           Api.Methods.Get.Comment
import           Api.Types.Comment
import           Api.Types.Response
import           Control.Exception              (try)
import           Control.Monad.Trans.Class      (MonadTrans (lift))
import           Data.Either                    (fromRight)
import           Data.Function                  ((&))
import           Data.Maybe                     (fromJust)
import           Data.Time.Calendar             (Day (ModifiedJulianDay))
import           Data.Time.LocalTime            (LocalTime (LocalTime),
                                                 midnight)
import qualified Database.Init                  as DB
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Transact   (DBT, getConnection)
import qualified MethodsTest.Author             as Author
import qualified MethodsTest.Category           as Category
import qualified MethodsTest.Draft              as Draft
import qualified MethodsTest.Tag                as Tag
import qualified MethodsTest.User               as User
import           Network.HTTP.Types.Status      (Status, status200, status400,
                                                 status403, status404)
import           Test.Hspec                     (Spec, SpecWith, hspec)
import           Test.Hspec.DB
import           Test.Hspec.Expectations.Lifted
import           TestHelper

spec :: Spec
spec =
  describeDB DB.init "Methods.Comment: " $ do
    User.createUserAdminAuthorAccounts
    Author.createAuthor1ByAdmin
    Tag.createFirstTagByAdmin
    Tag.createSecondTagByAdmin
    Category.createCategoryByAdmin
    Category.createChildCategoryByAdmin
    Draft.createDraftByAuthor1
    createCommentBeforePost
    Draft.publishDraft1
    createCommentAfterPost
    getComment
    deleteCommentByAuthor
    deleteCommentByUser
    getCommentAfterDelete
    createCommentAfterPost
    deleteCommentByAdmin
    getCommentAfterDelete

createCommentByUser = do
  conn <- getConnection
  token <- User.getUserToken conn
  lift $ try $ runWithState conn $ createComment (query token)
  where
    query token =
      [ ("news_id", Just "1")
      , ("content", Just "someContent1")
      , ("token", Just token)
      ]

createCommentBeforePost :: SpecWith TestDB
createCommentBeforePost =
  itDB "user can't post comment before draft is published" $ do
    res <- createCommentByUser
    res `shouldBe` (Left $ ErrorException status400 Err.noNews)

createCommentAfterPost :: SpecWith TestDB
createCommentAfterPost =
  itDB "user can post comment after draft is published" $ do
    res <- createCommentByUser
    (responseSuccess . snd <$> res) `shouldBe` Right True

getComment :: SpecWith TestDB
getComment =
  itDB "can get comment" $ do
    conn <- getConnection
    (status, resp) <- lift $ runWithState conn $ getComments query
    withDefTime_ (resp & responseResult) `shouldBe` Just [testComment1]
  where
    query = [("news_id", Just "1")]

deleteCommentByAuthor :: SpecWith TestDB
deleteCommentByAuthor =
  itDB "non admin or creator account can't delete comment" $ do
    conn <- getConnection
    token <- User.getAuthor1Token conn
    res <- lift $ try $ runWithState conn $ deleteComment (query token)
    res `shouldBe` (Left $ ErrorException status403 Err.noPerms)
  where
    query token = [("token", Just token), ("comment_id", Just "1")]

deleteCommentByUser :: SpecWith TestDB
deleteCommentByUser =
  itDB "user can delete own comment" $ do
    conn <- getConnection
    token <- User.getUserToken conn
    (status, resp) <- lift $ runWithState conn $ deleteComment (query token)
    (resp & responseSuccess) `shouldBe` True
  where
    query token = [("token", Just token), ("comment_id", Just "1")]

getCommentAfterDelete :: SpecWith TestDB
getCommentAfterDelete =
  itDB "comment is deleted" $ do
    conn <- getConnection
    (status, resp) <- lift $ runWithState conn $ getComments query
    (resp & responseResult) `shouldBe` Just []
  where
    query = [("news_id", Just "1")]

deleteCommentByAdmin :: SpecWith TestDB
deleteCommentByAdmin =
  itDB "admin can delete comment" $ do
    conn <- getConnection
    token <- User.getUserToken conn
    (status, resp) <- lift $ runWithState conn $ deleteComment (query token)
    (resp & responseSuccess) `shouldBe` True
  where
    query token = [("token", Just token), ("comment_id", Just "2")]

withDefTime :: Comment -> Comment
withDefTime comment@Comment {commentUser = user} =
  comment {commentCreationTime = defTime, commentUser = User.withDefTime user}

withDefTime_ :: Maybe [Comment] -> Maybe [Comment]
withDefTime_ = (fmap . fmap) withDefTime

testComment1 :: Comment
testComment1 = Comment 1 User.testUser defTime "someContent1"

testComment2 :: Comment
testComment2 = Comment 2 User.testUser defTime "someContent1"
