module UI.Model
    ( ChatMsg(..)
    , Model(..)
    , Msg(..)
    , initialState'
    ) where

import Data.Text (Text)

data ChatMsg = ChatMsg
    { msgText   :: Text
    , msgSender :: Text
    }

data Model = Model [ChatMsg]

data Msg
    = Quit
    | NewMsg ChatMsg

initialState' :: Model
initialState' = Model []
