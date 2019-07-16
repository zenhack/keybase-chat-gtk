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
        $ container Box [#orientation := OrientationVertical]
            [ BoxChild boxChildProps $ widget TextView [#editable := False]
            , container Box [#orientation := OrientationHorizontal]
                [ BoxChild boxChildProps $ widget Entry []
                , widget Button [#label := "Send"]
                ]
            ]


boxChildProps = defaultBoxChildProperties { expand = True, fill = True }
