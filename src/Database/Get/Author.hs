{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Database.Get.Author where

import           Api.ErrorException
import qualified Api.Errors                       as Err
import           Api.Types.Author
import           Api.Types.Synonyms
import           Control.Monad.Catch              (MonadThrow (throwM))
import           Control.Monad.Trans.Class        (MonadTrans (lift))
import           Control.Monad.Trans.State        (gets)
import           Database.Get.User
import           Database.PostgreSQL.Simple       (Connection, Only (Only),
                                                   execute, query)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Network.HTTP.Types               (status403)
import           State.Types

getAuthors ::
     Maybe AuthorId
  -> Maybe UserId
  -> Maybe Login
  -> Maybe FirstName
  -> Maybe LastName
  -> Maybe Limit
  -> Maybe Offset
  -> ServerStateIO [Author]
getAuthors mbAuthorId mbUserId mbLogin mbFName mbLName mbLimit mbOffset = do
  conn <- gets conn
  res <-
    lift $
    query
      conn
      [sql|
                SELECT author.id, author.description,
                usr.id, usr.login, usr.first_name, usr.last_name, usr.picture, date_trunc('second',usr.creation_time), usr.is_admin
                FROM author, user_account as usr
                WHERE author.id = COALESCE(?, author.id)
                AND author.user_id = COALESCE(?, author.user_id)
                AND usr.login = COALESCE(?, usr.login)
                AND usr.first_name = COALESCE(?, usr.first_name)
                AND usr.last_name = COALESCE(?, usr.last_name)
                AND author.user_id = usr.id
                LIMIT COALESCE(?, 50)
                OFFSET COALESCE(?, 0)
                |]
      (mbAuthorId, mbUserId, mbLogin, mbFName, mbLName, mbLimit, mbOffset)
  return $ map tupleToAuthor res

getAuthorIdOrThrow :: Token -> ServerStateIO AuthorId
getAuthorIdOrThrow token = do
  conn <- gets conn
  res <-
    lift $
    query
      conn
      [sql|
          SELECT author.id FROM user_token, user_account, author
          WHERE user_token.token = ?
          AND user_account.id = user_token.user_id
          AND author.user_id = user_account.id
          |]
      (Only token)
  case res of
    []        -> throwM $ ErrorException status403 Err.notAuthor
    [Only id] -> return id
