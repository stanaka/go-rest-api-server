{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where
import Control.Monad.Trans (MonadIO(..))
import Data.ByteString (ByteString)
import Data.Char (toLower)
import Data.IORef (IORef, newIORef, atomicModifyIORef')
import qualified Data.ByteString.Char8 as BS8

import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Resource (ResourceT)
import Data.Aeson.TH (deriveToJSON)
import Data.Pool (Pool, createPool, withResource)
import Data.MessagePack (derivePack)
import Database.MySQL.Simple.QueryResults (QueryResults(..))
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as AT
import qualified Data.MessagePack as MsgPack
import qualified Database.Memcache.Protocol as Memcache
import qualified Database.Memcache.Server as Memcached
import qualified Database.MySQL.Simple as MySql
import qualified Database.MySQL.Simple.QueryResults as MySql (convertError)
import qualified Database.MySQL.Simple.Result as MySql (convert)

data User = User
  { userName :: !ByteString
  , userMail :: !ByteString
  }

deriveToJSON AT.defaultOptions
  { AT.fieldLabelModifier = map toLower . drop 4 }
  ''User

-- MessagePack instances
derivePack True ''User

-- MySQL results instance
instance QueryResults User where
  convertResults [fName, fMail] [vName, vMail] = User
    (MySql.convert fName vName)
    (MySql.convert fMail vMail)
  convertResults fs vs = MySql.convertError fs vs 2

main :: IO ()
main = do
  uidRef <- newIORef 0
  mySqlConnPool <- createPool
    (MySql.connect MySql.defaultConnectInfo)
    MySql.close
    1 -- Stripe size
    0.5 -- Timeout in seconds
    1 -- Maximum number of connections
  memcachedConn <- Memcached.newMemcacheClient "localhost" 11211
  runSettings settings $ server uidRef mySqlConnPool memcachedConn
  where
    settings = defaultSettings
      { settingsPort = 9000
      }

server
  :: IORef Int
  -> Pool MySql.Connection
  -> Memcached.Connection
  -> Application
server uidRef mySqlConnPool memcachedConn req = do
  case pathInfo req of
    [] -> msgpackHandler uidRef mySqlConnPool
    ["json"] -> jsonHandler uidRef mySqlConnPool
    ["mem"] -> msgpackMemcachedHandler uidRef memcachedConn
    ["mem.json"] -> jsonMemcachedHandler uidRef memcachedConn
    _ -> notFound

msgpackHandler
  :: IORef Int
  -> Pool MySql.Connection
  -> ResourceT IO Response
msgpackHandler uidRef pool = do
  user <- fetchMySql uidRef pool
  return $ msgPackResponse user

jsonHandler
  :: IORef Int
  -> Pool MySql.Connection
  -> ResourceT IO Response
jsonHandler uidRef pool = do
  user <- fetchMySql uidRef pool
  return $ jsonResponse user

msgpackMemcachedHandler
  :: IORef Int
  -> Memcached.Connection
  -> ResourceT IO Response
msgpackMemcachedHandler uidRef conn = do
  user <- liftIO $ fetchMemcached uidRef conn
  return $ msgPackResponse user

jsonMemcachedHandler
  :: IORef Int
  -> Memcached.Connection
  -> ResourceT IO Response
jsonMemcachedHandler uidRef conn = do
  user <- liftIO $ fetchMemcached uidRef conn
  return $ jsonResponse user

notFound :: ResourceT IO Response
notFound = return $ responseLBS status404
  [(hContentType, "text/plain")]
  "Not found"

fetchMySql
  :: (MonadIO m, MonadBaseControl IO m)
  => IORef Int
  -> Pool MySql.Connection
  -> m User
fetchMySql uidRef pool = do
  uid <- liftIO $ incrementUid uidRef
  results <- withResource pool $ \conn ->
    liftIO $ MySql.query conn
      "SELECT name, mail FROM user WHERE id=?"
      (MySql.Only uid)
  case results of
    [] -> fail "User not found"
    user:_ -> return user

fetchMemcached :: IORef Int -> Memcached.Connection -> IO User
fetchMemcached uidRef conn = do
  uid <- incrementUid uidRef
  result <- Memcache.get conn (intToBS uid)
  case result of
    Nothing -> fail "User not found"
    Just (v, _, _) -> case BS8.split '|' v of
      [name, mail] -> return $ User name mail
      _ -> fail "Failed to parse response"
  where
    intToBS :: Int -> ByteString
    intToBS = BS8.pack . show

incrementUid :: IORef Int -> IO Int
incrementUid ref = atomicModifyIORef' ref update
  where
    update uid = (nextUid, nextUid)
      where
        nextUid
          | uid >= 10000 = 0
          | otherwise = uid + 1

msgPackResponse :: MsgPack.Packable a => a -> Response
msgPackResponse = responseLBS status202
  [(hContentType, "application/x-msgpack")]
  . MsgPack.pack

jsonResponse :: A.ToJSON a => a -> Response
jsonResponse = responseLBS status202
  [(hContentType, "application/json")]
  . A.encode
