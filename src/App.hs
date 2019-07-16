{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists  #-}
module App (main) where

import Zhp

import GI.Gtk                        hiding ((:=), main, on)
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple

data Model = Model

data Msg
    = Quit

main :: IO ()
main = void $ run App
    { view = view'
    , update = update'
    , inputs = []
    , initialState = Model
    }

update' :: Model -> Msg -> Transition Model Msg
update' Model Quit = Exit

view' :: Model -> AppView Window Msg
view' Model =
    bin Window
        [ #title := "Hello, Keybase!"
        , on #destroy Quit
        ]
        $ paned [#orientation := OrientationVertical]
            (pane defaultPaneProperties $ widget TextView [])
            (pane defaultPaneProperties $
                container Box [#orientation := OrientationHorizontal]
                    [ BoxChild defaultBoxChildProperties { expand = True, fill = True } $
                        widget TextView []
                    , widget Button [#label := "Send"]
                    ]
            )
