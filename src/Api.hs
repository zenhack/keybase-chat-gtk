-- Intract with the keybase chat API.
--
-- For this to work, the 'keybase' command line tool must be in $PATH;
-- this operates by launching `keybase chat api` and talking to it via its
-- stdio.
module Api
    ( -- * Connecting to the API.
      Conn
    , withConn
    ) where

import Zhp

import Control.Monad.IO.Unlift (MonadUnliftIO)
import System.Process.Typed
    (Process, createPipe, proc, setStdin, setStdout, withProcessWait)

import qualified Data.Aeson as A

-- A Connection to the API
newtype Conn = Conn
    { connApiProc :: Process Handle Handle ()
    }

-- | @'withConn' f@ connects to the API, runs the action passing it the connection,
-- and then shuts down the connection.
withConn :: MonadUnliftIO m => (Conn -> m a) -> m a
withConn f =
    let cfg = proc "keybase" ["chat", "api"]
            & setStdin createPipe
            & setStdout createPipe
    in
    withProcessWait cfg $ \proc -> f (Conn proc)

data MethodCall = MethodCall
