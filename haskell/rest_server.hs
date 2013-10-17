{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where
import Control.Applicative ((<$>), (<*>))
import Control.Monad.Trans (liftIO)
import Data.ByteString (ByteString)
import Data.IORef (IORef, newIORef, atomicModifyIORef')
import qualified Data.ByteString.Char8 as BS8

import Control.Monad.Trans.Resource (ResourceT)
import Data.Aeson.TH (deriveToJSON)
import Data.MessagePack (derivePack)
import Database.Memcache.Server as Memcached
import Database.MySQL.Simple.QueryResults (QueryResults(..))
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as AT
import qualified Data.MessagePack as MsgPack
import qualified Database.Memcache.Protocol as Memcache
import qualified Database.MySQL.Simple as MySql
import qualified Database.MySQL.Simple.Result as MySql (convert)
import qualified Database.MySQL.Simple.QueryResults as MySql (convertError)

data User = User
  { userName :: !ByteString
  , userMail :: !ByteString
  }

deriveToJSON AT.defaultOptions
  { AT.fieldLabelModifier = drop 4 }
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
  mySqlConn <- MySql.connect MySql.defaultConnectInfo
  memcachedConn <- newMemcacheClient "localhost" 11211
  runSettings settings $ server uidRef mySqlConn memcachedConn
  where
    settings = defaultSettings
      { settingsPort = 9000
      }

server
  :: IORef Int
  -> MySql.Connection
  -> Memcached.Connection
  -> Application
server uidRef mySqlConn memcachedConn req = do
  case pathInfo req of
    [] -> msgpackHandler uidRef mySqlConn
    ["json"] -> jsonHandler uidRef mySqlConn
    ["mem"] -> msgpackMemcachedHandler uidRef memcachedConn
    ["mem.json"] -> jsonMemcachedHandler uidRef memcachedConn
    _ -> notFound

msgpackHandler
  :: IORef Int
  -> MySql.Connection
  -> ResourceT IO Response
msgpackHandler uidRef conn = do
  user <- liftIO $ fetchMySql uidRef conn
  return $ msgPackResponse user

jsonHandler
  :: IORef Int
  -> MySql.Connection
  -> ResourceT IO Response
jsonHandler uidRef conn = do
  user <- liftIO $ fetchMySql uidRef conn
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

fetchMySql :: IORef Int -> MySql.Connection -> IO User
fetchMySql uidRef conn = do
  uid <- incrementUid uidRef
  results <- MySql.query conn
      "SELECT name, mail FROM user WHERE id=?"
      (MySql.Only uid)
  case results of
    [] -> fail "User not found"
    [user] -> return user

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
          | uid > 10000 = 0
          | otherwise = uid + 1

msgPackResponse :: MsgPack.Packable a => a -> Response
msgPackResponse = responseLBS status202
  [(hContentType, "application/x-msgpack")]
  . MsgPack.pack

jsonResponse :: A.ToJSON a => a -> Response
jsonResponse = responseLBS status202
  [(hContentType, "application/json")]
  . A.encode
