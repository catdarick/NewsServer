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
import qualified MethodsTest.User as MethodsUser
import qualified MethodsTest.Author as MethodsAuthor
import qualified MethodsTest.Category as MethodsCategory
import qualified MethodsTest.Tag as MethodsTag
import qualified MethodsTest.Draft as MethodsDraft
import qualified MethodsTest.News as MethodsNews
import qualified MethodsTest.Comment as MethodsComment
main :: IO ()
main = hspec $ do
 DbUser.spec
 DbAuthor.spec
 DbCategory.spec
 DbTag.spec
 DbDraft.spec
 DbNews.spec
 DbComment.spec
 MethodsUser.spec
 MethodsAuthor.spec
 MethodsCategory.spec
 MethodsTag.spec
 MethodsDraft.spec
 MethodsNews.spec
 MethodsComment.spec



