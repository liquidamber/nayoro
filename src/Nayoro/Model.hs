{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances #-}

module
  Nayoro.Model
  where

import qualified Data.Conduit as CD
import Data.Text (Text)
import Data.Time.Clock (UTCTime(..))
import Database.Persist.TH
import Database.Persist.Sql (SqlPersistT)
import qualified Database.Esqueleto as E
import Database.Esqueleto ((^.), (?.))
import Control.Monad.Trans.Resource (MonadResource)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person json
  mail Text
  UniqueMail mail
  deriving Show

Handle json
  personId PersonId
  service Text
  name Text
  created UTCTime
  UniqueHandle personId service created
  deriving Show

Uri json
  handleId HandleId
  uri Text
  label Text
  UniqueUri handleId label
  deriving Show
|]

{-
SELECT * FROM
  person
  INNER JOIN handle ON person.id == handle.person_id
  LEFT OUTER JOIN uri ON handle.id == uri.handle_id
  WHERE EXISTS (
    SELECT * FROM handle AS h2
    WHERE person.id == h2.person_id
      AND (person.mail LIKE "%white%" OR h2.name LIKE "%white%"));
-}
getHandleAndUris :: MonadResource m => Text -> CD.Source (SqlPersistT m) (E.Entity Person, E.Entity Handle, Maybe (E.Entity Uri))
getHandleAndUris query =
  E.selectSource $ E.from $ \(person `E.InnerJoin` handle `E.LeftOuterJoin` muri) -> do
    E.on $ E.just (handle ^. HandleId) E.==. muri ?. UriHandleId
    E.on $ person ^. PersonId E.==. handle ^. HandlePersonId
    E.where_ $ E.exists $ E.from $ \handle -> do
      E.where_ $
        (person ^. PersonId E.==. handle ^. HandlePersonId)
        E.&&. ((person ^. PersonMail `E.like` (E.%) E.++. E.val query E.++. (E.%))
               E.||. (handle ^. HandleName `E.like` (E.%) E.++. E.val query E.++. (E.%)))
    E.orderBy
      [ E.asc (person ^. PersonMail)
      , E.asc (handle ^. HandleService)
      , E.asc (handle ^. HandleName)
      , E.asc (muri ?. UriLabel)
      ]
    return (person, handle, muri)
