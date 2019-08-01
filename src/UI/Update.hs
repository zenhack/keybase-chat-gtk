module UI.Update
    ( update'
    ) where

import UI.Model

import GI.Gtk.Declarative.App.Simple

update' :: Model -> Msg -> Transition Model Msg
update' _ Quit = Exit
