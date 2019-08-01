{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists  #-}
module App (main) where

import Zhp

import Control.Concurrent.Async (concurrently_)
import Control.Concurrent.STM
import Pipes                    (yield)

import GI.Gtk                        hiding ((:=), Widget, main, on)
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple

import           UI.Model
import qualified UI.Update
import qualified UI.View

main :: IO ()
main = do
    ch <- atomically newTChan
    concurrently_
        (runUI ch) (runDaemon ch)

runUI ch =
    void $ run App
        { view = UI.View.view'
        , update = UI.Update.update'
        , inputs =
            [ forever $ liftIO (atomically (readTChan ch)) >>= yield
            ]
        , initialState = initialState'
        }

runDaemon ch =
    traverse_ (atomically . writeTChan ch . NewMsg) initMsgs
  where
    initMsgs :: [ChatMsg]
    initMsgs =
        [ ChatMsg
            { msgText = "Hey, Bob!"
            , msgSender = "Alice"
            }
        , ChatMsg
            { msgText = "Hey Alice!"
            , msgSender = "Bob"
            }
        ]
