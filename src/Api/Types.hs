{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE TemplateHaskell            #-}
-- | Types that appear in the keybase chat API.
module Api.Types
    ( ListResult(..)
    , Conversation(..)
    , Time(..)
    , ConversationId(..)
    , Channel(..)
    , MembersType(..)
    , TopicType(..)
    , MsgId(..)
    , Msg(..)
    , Uid(..)
    , DeviceId(..)
    , Sender(..)
    ) where

import Zhp

import qualified Data.Aeson      as A
import qualified Data.Aeson.TH   as A
import           Data.List       (intercalate)
import           Data.List.Extra (wordsBy)
import           Data.Text       (Text)
import           Prelude         (tail)

newtype Time
    = Time Int64
    deriving(A.ToJSON, A.FromJSON, Show, Read, Eq, Ord, Bounded)

data ListResult = ListResult
    { listresConversations :: [Conversation]
    , listresOffline       :: Bool
    }
    deriving(Show, Read, Eq)

newtype ConversationId
    = ConversationId Text
    deriving(A.ToJSON, A.FromJSON, Show, Read, Eq, Ord)

data Conversation = Conversation
    { convId      :: ConversationId
    , convChannel :: Channel
    , convUnread  :: Bool
    }
    deriving(Show, Read, Eq)

data Channel = Channel
    { chanName        :: Text
    , chanPublic      :: Bool
    , chanMembersType :: MembersType
    , chanTopicType   :: TopicType
    , chanTopicName   :: Maybe Text
    }
    deriving(Show, Read, Eq)

-- TODO: I(zenhack) assume there is a more specific set of values that MembersType
-- and TopicType can take on; figure out what these are  and make them simple sum
-- types instead of wrappers around Text.
newtype MembersType
    = MembersType Text
    deriving(A.ToJSON, A.FromJSON, Show, Read, Eq, Ord)

newtype TopicType
    = TopicType Text
    deriving(A.ToJSON, A.FromJSON, Show, Read, Eq, Ord)

newtype MsgId
    = MsgId Int64
    deriving(A.ToJSON, A.FromJSON, Show, Read, Eq, Ord, Bounded)

data Msg = Msg
    { msgId             :: MsgId
    , msgConversationId :: ConversationId
    , msgChannel        :: Channel
    , msgSender         :: Sender
    , msgSentAt         :: Time
    }
    deriving(Show, Read, Eq)

newtype Uid
    = Uid Text
    deriving(A.ToJSON, A.FromJSON, Show, Read, Eq, Ord)

newtype DeviceId
    = DeviceId Text
    deriving(A.ToJSON, A.FromJSON, Show, Read, Eq, Ord)

data Sender = Sender
    { senderUid        :: Uid
    , senderUsername   :: Text
    , senderDeviceId   :: DeviceId
    , senderDeviceName :: Text
    }
    deriving(Show, Read, Eq)

-- Derive JSON type classes:
do  let drv =
            A.deriveJSON A.defaultOptions
                { A.fieldLabelModifier = mangleFieldLabel
                , A.omitNothingFields = True
                }

        mangleFieldLabel =
            toSnakeCase >>> dropPrefix

        toSnakeCase "" = ""
        toSnakeCase (c:cs)
            | isUpper c = '_' : toLower c : toSnakeCase cs
            | otherwise = c : toSnakeCase cs

        dropPrefix =
            wordsBy (== '_')
            >>> tail
            >>> intercalate "_"

    mconcat <$> traverse drv
        -- keep these sorted:
        [ 'Channel
        , 'Conversation
        , 'ListResult
        , 'Msg
        , 'Sender
        ]
