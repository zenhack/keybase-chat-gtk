{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists  #-}
module App (main) where

import Zhp

import           Data.Text   (Text)
import qualified Data.Vector as V

import GI.Gtk                        hiding ((:=), Widget, main, on)
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple

data ChatMsg = ChatMsg
    { msgText   :: Text
    , msgSender :: Text
    }

data Model = Model [ChatMsg]

data Msg
    = Quit

initialState' :: Model
initialState' = Model
    [ ChatMsg
        { msgText = "Hey, Bob!"
        , msgSender = "Alice"
        }
    , ChatMsg
        { msgText = "Hey Alice!"
        , msgSender = "Bob"
        }
    ]

main :: IO ()
main = void $ run App
    { view = view'
    , update = update'
    , inputs = []
    , initialState = initialState'
    }

update' :: Model -> Msg -> Transition Model Msg
update' _ Quit = Exit

view' :: Model -> AppView Window Msg
view' (Model msgs) =
    bin Window
        [ #title := "Hello, Keybase!"
        , on #destroy Quit
        ]
        $ container Box [#orientation := OrientationVertical]
            [ BoxChild boxChildProps $
                container Box [#orientation := OrientationVertical] $
                    fmap
                        (BoxChild boxChildProps { expand = False } . viewMsg)
                        (V.fromList msgs)
            , container Box [#orientation := OrientationHorizontal]
                [ BoxChild boxChildProps $ widget Entry []
                , widget Button [#label := "Send"]
                ]
            ]

viewMsg :: ChatMsg -> Widget Msg
viewMsg msg =
    container Box [#orientation := OrientationHorizontal]
        [ BoxChild defaultBoxChildProperties $ widget Label [#label := msgSender msg]
        , BoxChild boxChildProps $ widget Label [#label := msgText msg]
        ]


boxChildProps = defaultBoxChildProperties { expand = True, fill = True }
