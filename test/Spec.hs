import           Test.Hspec                     (Spec, hspec)
import           Test.Hspec.Expectations.Lifted
import           Test.Hspec.DB
import           Control.Monad
import qualified DatabaseTest.User as DbUser
import qualified DatabaseTest.Author as DbAuthor
import qualified DatabaseTest.Category as DbCategory
import qualified DatabaseTest.Tag as DbTag
import qualified DatabaseTest.Draft as DbDraft
import qualified DatabaseTest.News as DbNews
import qualified DatabaseTest.Comment as DbComment
main :: IO ()
main = hspec $ do
  DbUser.spec
  DbAuthor.spec
  DbCategory.spec
  DbTag.spec
  DbDraft.spec
  DbNews.spec
  DbComment.spec



