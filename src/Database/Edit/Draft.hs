{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.Edit.Draft where

import           Api.Types.Synonyms
import           Control.Monad                    (void, when)
import           Control.Monad.Trans.Class        (MonadTrans (lift))
import           Control.Monad.Trans.State        (gets)
import           Data.Maybe                       (isJust)
import           Data.Vector                      (fromList)
import           Database.Create.Draft
import           Database.Delete.Draft
import           Database.PostgreSQL.Simple       (Connection, execute)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           State.Types

editDraft ::
     NewsId
  -> Maybe Title
  -> Maybe Content
  -> Maybe CategoryId
  -> Maybe Picture
  -> Maybe [Picture]
  -> Maybe [TagId]
  -> ServerStateIO ()
editDraft draftId mbTitle mbContent mbCategoryId mbPicture mbPictures mbTagsId = do
  conn <- gets conn
  lift $
    execute
      conn
      [sql|
      UPDATE news
      SET
      title = COALESCE(?, title),
      content = COALESCE(?, content),
      category_id = COALESCE(?, category_id),
      main_picture = COALESCE(?, main_picture),
      pictures = COALESCE(?, pictures)
      WHERE id=?|]
      ( mbTitle
      , mbContent
      , mbCategoryId
      , mbPicture
      , fromList <$> mbPictures
      , draftId)
  when (isJust mbTagsId) $
    void $ do
      deleteDraftTags draftId
      addDraftTags draftId mbTagsId
