{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Database.Checks.Draft where

import           Api.ErrorException
import qualified Api.Errors                       as Err
import           Api.Types.Synonyms
import           Control.Monad.Catch              (MonadThrow (throwM))
import           Control.Monad.Trans.Class        (MonadTrans (lift))
import           Control.Monad.Trans.State        (gets)
import           Database.Checks.User
import           Database.Get.Draft
import           Database.PostgreSQL.Simple       (Connection, Only (Only),
                                                   query)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Network.HTTP.Types               (status403)
import           State.Types

isDraftExists :: NewsId -> ServerStateIO Bool
isDraftExists newsId = do
  conn <- gets conn
  res <-
    lift $
    query
      conn
      [sql|
        SELECT id FROM news
        WHERE id = ?
        |]
      (Only newsId)
  case res of
    []               -> return False
    [id :: Only Int] -> return True
  return True

isDraftAuthorToken :: NewsId -> Token -> ServerStateIO Bool
isDraftAuthorToken newsId token = do
  conn <- gets conn
  res <-
    lift $
    query
      conn
      [sql|
        SELECT user_token.user_id FROM user_token, author, news
        WHERE news.id = ?
        AND news.author_id = author.id
        AND author.user_id = user_token.user_id
        AND user_token.token = ?
        |]
      (newsId, token)
  case res of
    []               -> return False
    [id :: Only Int] -> return True

isDraftPublished :: NewsId -> ServerStateIO Bool
isDraftPublished newsId = do
  conn <- gets conn
  res <-
    lift $
    query
      conn
      [sql|
        SELECT id FROM news
        WHERE id = ?
        AND is_published=true
        |]
      (Only newsId)
  case res of
    []               -> return False
    [id :: Only Int] -> return True

draftAuthorGuard :: NewsId -> Token -> ServerStateIO ()
draftAuthorGuard draftId token = do
  isDraftAuthor <- isDraftAuthorToken draftId token
  if isDraftAuthor
    then return ()
    else throwM $ ErrorException status403 Err.noPerms

adminOrAuthorGuard :: NewsId -> Token -> ServerStateIO ()
adminOrAuthorGuard draftId token = do
  isAdmin <- isAdminToken token
  isDraftAuthor <- isDraftAuthorToken draftId token
  if isDraftAuthor || isAdmin
    then return ()
    else throwM $ ErrorException status403 Err.noPerms
