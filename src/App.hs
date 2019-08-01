{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists  #-}
module App (main) where

import Zhp

import qualified Api
import qualified Api.Types as AT

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

runDaemon ch = Api.withConn $ \conn -> do
    (c:_) <- AT.listresConversations <$> Api.list conn
    msgs <- Api.peekConv conn (AT.convId c)
    traverse_
        (atomically . writeTChan ch . NewMsg)
        [ m | Just m <- map convertMsg msgs]
  where
    convertMsg :: AT.Msg -> Maybe ChatMsg
    convertMsg AT.Msg
        { AT.msgSender = AT.Sender { AT.senderUsername = username }
        , AT.msgContent = AT.MsgText (AT.ContentText text)
        }
        = Just ChatMsg
            { msgText = text
            , msgSender = username
            }
    convertMsg _ = Nothing
