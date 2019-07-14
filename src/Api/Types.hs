{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Types that appear in the keybase chat API.
module Api.Types
    ( Conversation
    ) where

import Zhp

import qualified Data.Aeson as A
import           Data.Text  (Text)

newtype Time
    = Time Int64
    deriving(A.ToJSON, A.FromJSON)

data ListResult = ListResult
    { listresCoversations :: [Conversation]
    , listresOffline      :: Bool
    }

newtype ConversationId
    = ConversationId Text
    deriving(A.ToJSON, A.FromJSON)

data Conversation = Conversation
    { convId      :: ConversationId
    , convChannel :: Channel
    , convUnread  :: Bool
    }

data Channel = Channel
    { chanName        :: Text
    , chanPublic      :: Bool
    , chanMembersType :: MembersType
    , chanTopicType   :: TopicType
    , chanTopicName   :: Maybe Text
    }

-- TODO: I(zenhack) assume there is a more specific set of values that MembersType
-- and TopicType can take on; figure out what these are  and make them simple sum
-- types instead of wrappers around Text.
newtype MembersType
    = MembersType Text
    deriving(A.ToJSON, A.FromJSON)

newtype TopicType
    = TopicType Text
    deriving(A.ToJSON, A.FromJSON)

newtype MsgId
    = MsgId Int64
    deriving(A.ToJSON, A.FromJSON)

data Msg = Msg
    { msgId             :: MsgId
    , msgConversationId :: ConversationId
    , msgChannel        :: Channel
    , msgSender         :: Sender
    , msgSentAt         :: Time
    }

newtype Uid
    = Uid Text
    deriving(A.ToJSON, A.FromJSON)

newtype DeviceId
    = DeviceId Text
    deriving(A.ToJSON, A.FromJSON)

data Sender = Sender
    { senderUid        :: Uid
    , senderUsername   :: Text
    , senderDeviceId   :: DeviceId
    , senderDeviceName :: Text
    }
