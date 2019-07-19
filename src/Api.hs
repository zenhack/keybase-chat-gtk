-- Intract with the keybase chat API.
--
-- For this to work, the 'keybase' command line tool must be in $PATH;
-- this operates by launching `keybase chat api` and talking to it via its
-- stdio.
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Api
    ( -- * Connecting to the API.
      Conn
    , withConn
    ) where

import Zhp

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import GHC.Generics            (Generic)
import System.Process.Typed
    ( Process
    , createPipe
    , getStdin
    , getStdout
    , proc
    , setStdin
    , setStdout
    , withProcessWait
    )

import qualified Api.Types as AT

import           Data.Aeson           ((.=))
import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE

-- A Connection to the API
data Conn = Conn
    { connApiProc :: Process Handle Handle ()
    , connMutex   :: MVar ()
    }

-- | @'withConn' f@ connects to the API, runs the action passing it the connection,
-- and then shuts down the connection.
withConn :: MonadUnliftIO m => (Conn -> m a) -> m a
withConn f =
    let cfg = proc "keybase" ["chat", "api"]
            & setStdin createPipe
            & setStdout createPipe
    in
    withProcessWait cfg $ \proc -> do
        mu <- liftIO $ newMVar ()
        f Conn
            { connApiProc = proc
            , connMutex = mu
            }

call :: (MonadIO m, A.ToJSON req, A.FromJSON resp) => Conn -> req -> m resp
call Conn{ connApiProc = proc, connMutex = mu } req =
    liftIO $ withMVar mu $ \_ -> do
        LBS.hPut (getStdin proc) (A.encode req <> "\n")
        hFlush (getStdin proc)
        -- XXX: this is pretty gross; ideally we'd just read in the line
        -- and hand it straight to decode, but I can't find a function that
        -- reads a line and returns it as a ByteString.
        line <- hGetLine (getStdout proc)
        case A.decodeStrict $ TE.encodeUtf8 (T.pack line) of
            Just x  -> pure x
            Nothing -> error "TODO: throw a proper exception"

data MethodCall param = MethodCall
    { method :: T.Text
    , param  :: param
    }
    deriving(Generic)

instance A.FromJSON a => A.FromJSON (MethodCall a)
instance A.ToJSON a => A.ToJSON (MethodCall a)

newtype MethodReturn ret = MethodReturn
    { result :: ret
    }
    deriving(A.FromJSON, A.ToJSON)

list :: MonadIO m => Conn -> m AT.ListResult
list conn =
    result <$> call conn (A.object ["method" .= A.String "list"])
