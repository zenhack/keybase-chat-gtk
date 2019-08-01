module UI.Update
    ( update'
    ) where

import Zhp

import UI.Model

import GI.Gtk.Declarative.App.Simple

update' :: Model -> Msg -> Transition Model Msg
update' _ Quit = Exit
update' (Model msgs) (NewMsg msg) =
    Transition
        (Model (msg : msgs))
        (pure Nothing)
