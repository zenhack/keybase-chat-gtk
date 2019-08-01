{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists  #-}
module UI.View
    ( view'
    ) where

import Zhp

import           Data.Default (def)
import qualified Data.Vector  as V

import UI.Model

import GI.Gtk                        hiding ((:=), Widget, main, on)
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple

view' :: Model -> AppView Window Msg
view' (Model msgs) =
    bin Window
        [ #title := "Hello, Keybase!"
        , on #destroy Quit
        ]
        $ container Box [#orientation := OrientationVertical]
            [ BoxChild boxChildProps $
                bin ScrolledWindow [] $
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
        [ BoxChild def $ widget Label [#label := msgSender msg]
        , BoxChild boxChildProps $ widget Label [#label := msgText msg]
        ]


boxChildProps = defaultBoxChildProperties { expand = True, fill = True }
