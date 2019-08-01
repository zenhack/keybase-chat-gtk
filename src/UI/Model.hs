module UI.Model
    ( ChatMsg(..)
    , Model(..)
    , Msg(..)
    , initialState
    ) where

import Data.Text (Text)

data ChatMsg = ChatMsg
    { msgText   :: Text
    , msgSender :: Text
    }

data Model = Model [ChatMsg]

data Msg
    = Quit

initialState :: Model
initialState = Model
    [ ChatMsg
        { msgText = "Hey, Bob!"
        , msgSender = "Alice"
        }
    , ChatMsg
        { msgText = "Hey Alice!"
        , msgSender = "Bob"
        }
    ]
