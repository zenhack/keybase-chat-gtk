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
    deriving stock (Show, Read, Eq, Ord, Bounded)
    deriving newtype (A.ToJSON, A.FromJSON)

data ListResult = ListResult
    { listresCoversations :: [Conversation]
    , listresOffline      :: Bool
    }
    deriving(Show, Read, Eq)

newtype ConversationId
    = ConversationId Text
    deriving stock (Show, Read, Eq, Ord)
    deriving newtype (A.ToJSON, A.FromJSON)

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
    deriving stock (Show, Read, Eq, Ord)
    deriving newtype (A.ToJSON, A.FromJSON)

newtype TopicType
    = TopicType Text
    deriving stock (Show, Read, Eq, Ord)
    deriving newtype (A.ToJSON, A.FromJSON)

newtype MsgId
    = MsgId Int64
    deriving stock (Show, Read, Eq, Ord, Bounded)
    deriving newtype (A.ToJSON, A.FromJSON)

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
    deriving stock (Show, Read, Eq, Ord)
    deriving newtype (A.ToJSON, A.FromJSON)

newtype DeviceId
    = DeviceId Text
    deriving stock (Show, Read, Eq, Ord)
    deriving newtype (A.ToJSON, A.FromJSON)

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

    -- keep these sorted:
    drv 'Channel
    drv 'Conversation
    drv 'ListResult
    drv 'Msg
    drv 'Sender
