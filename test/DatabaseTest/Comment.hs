{-# LANGUAGE OverloadedStrings #-}

module DatabaseTest.Comment where

import           Api.Types.Comment
import           Control.Monad
import           Control.Monad.Trans.Class      (MonadTrans (lift))
import           Data.Function                  ((&))
import           Data.Time                      (Day (ModifiedJulianDay),
                                                 LocalTime (LocalTime),
                                                 midnight)
import           Database.Create.Comment
import           Database.Get.Comment
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Transact   (getConnection)
import qualified DatabaseTest.Author            as Author
import qualified DatabaseTest.Category          as Category
import qualified DatabaseTest.Draft             as Draft
import qualified DatabaseTest.Tag               as Tag
import qualified DatabaseTest.User              as User
import qualified Database.Init                  as DB
import           Test.Hspec                     (Spec, SpecWith, hspec)
import           Test.Hspec.DB
import           Test.Hspec.Expectations.Lifted

spec :: Spec
spec =
  describeDB DB.init "Comment: " $ do
    Author.insert
    Category.insert
    Tag.insert
    Draft.insert
    Draft.post
    create
    get

create :: SpecWith TestDB
create =
  itDB "create" $ do
    conn <- getConnection
    res <- lift $ addComment conn 1 1 "someText"
    res `shouldBe` 1

get :: SpecWith TestDB
get =
  itDB "can get by category id" $ do
    conn <- getConnection
    res <- lift $ getComments conn 1 Nothing Nothing
    (withDefTime <$> res) `shouldBe` [testComment]

testComment :: Comment
testComment = Comment 1 Author.testUser defTime "someText"

defTime :: LocalTime
defTime = LocalTime (ModifiedJulianDay 0) midnight

withDefTime :: Comment -> Comment
withDefTime comment@Comment {commentUser = user} =
  comment {commentCreationTime = defTime, commentUser = User.withDefTime user}
