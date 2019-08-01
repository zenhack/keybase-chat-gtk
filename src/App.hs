{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists  #-}
module App (main) where

import Zhp

import GI.Gtk                        hiding ((:=), Widget, main, on)
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple

import qualified UI.Model
import qualified UI.Update
import qualified UI.View

main :: IO ()
main = void $ run App
    { view = UI.View.view'
    , update = UI.Update.update'
    , inputs = []
    , initialState = UI.Model.initialState
    }
