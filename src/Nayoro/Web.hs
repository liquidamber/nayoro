{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module
  Nayoro.Web
  ( runApp
  ) where

import Web.Apiary
import Web.Apiary.Database.Persist
import Web.Apiary.Logger
import Network.Wai.Handler.Warp
import           Control.Monad.IO.Class  (liftIO)
import Control.Monad.Trans.Class  (lift)
import qualified Data.Aeson as A
import           Data.Conduit (await, ($$), ($=))
import qualified Data.Conduit.List as CL
import           Database.Persist
import           Database.Persist.Sqlite
import           Data.Time
import Control.Monad.Trans.Resource (liftResourceT, runResourceT, MonadResource)
import qualified Data.Vector as V

import qualified Data.ByteString.Lazy.Char8 as L

import Nayoro.Model
import Nayoro.Util
import qualified Nayoro.Config as Conf

instance MonadResource m => MonadResource (ActionT exts prms m) where
  liftResourceT = lift . liftResourceT

mkStub :: IO ()
mkStub = do
  runSqlite "test.sqlite" $ do
    time <- liftIO getCurrentTime
    runMigration migrateAll

    johnId <- insert $ Person "john@example.com"
    janeId <- insert $ Person "jane@example.com"
    johnWId <- insert $ Person "john_watson@example.com"

    johnExampleId <- insert $ Handle johnId "example" "John Doe" time
    janeExampleId <- insert $ Handle janeId "example" "Jane Doe" time
    insert $ Handle johnId "irc" "john_doe" time
    johnAnotherId <- insert $ Handle johnWId "another" "John Watson" time
    insert $ Handle johnWId "irc" "xwhitex" time

    insert $ Uri johnExampleId "calendar" "http://example.com/calendar/john"
    insert $ Uri johnExampleId "address book" "http://example.com/address/john"
    insert $ Uri johnAnotherId "irc" "http://example.com/calendar/john_watson"

    liftIO $ putStrLn "get watson from white"
    getHandleAndUris "white" $$ CL.mapM_ (liftIO . print)

runApp :: Conf.AppConfig -> IO ()
runApp _= do
  mkStub
  runApiaryTWith runResourceT (run 3000)
    ( initLogger def
      +> initPersistPool (withSqlitePool "test.sqlite" 10) migrateAll
     ) def $ do
    [capture|/|] . method GET . action $ do
      contentType "text/html"
      mapM_ lazyBytes ["<h1>Hello, World!</h1>\n"]

    [capture|/age::Int|] . ([key|name|] =: pLazyByteString) . method GET . action $ do
      (age, name) <- [params|age,name|]
      guard (age >= 18)
      contentType "text/html"
      mapM_ lazyBytes ["<h1>Hello, ", name, "!</h1>\n"]

    [capture|/search|] . ([key|q|] =: pText) . method GET . action $ do
      query <- param [key|q|]
      contentType "text/plain"
      v <- runSql $ getHandleAndUris query $= CL.mapM (lift . lift . return . A.toJSON) $$ sinkVector
      lazyBytes $ A.encode (v :: V.Vector A.Value)
