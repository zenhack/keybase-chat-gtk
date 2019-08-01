{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE TemplateHaskell            #-}
-- | Types that appear in the keybase chat API.
module Api.Types
    ( Channel(..)
    , ContentAttachment(..)
    , ContentDelete(..)
    , ContentHeadline(..)
    , ContentMetadata(..)
    , ContentReaction(..)
    , ContentSystem(..)
    , ContentText(..)
    , ContentUnfurl(..)
    , Conversation(..)
    , ListResult(..)
    , Msg(..)
    , MsgContent(..)
    , MsgWrapper(..)
    , ReadResult(..)
    , Sender(..)

    , Time(..)
    , MembersType(..)
    , TopicType(..)
    , ConversationId(..)
    , MsgId(..)
    , Uid(..)
    , DeviceId(..)
    ) where

import Zhp

import qualified Data.Aeson      as A
import qualified Data.Aeson.TH   as A
import           Data.List       (intercalate)
import           Data.List.Extra (wordsBy)
import           Data.Text       (Text)
import           Prelude         (tail)

-- TODO: would be nice for this to wrap something from the time package;
-- I(zenhack) just haven't gotten around to figuring out what.
newtype Time
    = Time Int64
    deriving(A.ToJSON, A.FromJSON, Show, Read, Eq, Ord, Bounded)

-- | Results of the "list" method call.
data ListResult = ListResult
    { listresConversations :: [Conversation]
    , listresOffline       :: Bool
    }
    deriving(Show, Read, Eq)

-- | Results of read/peek message calls.
data ReadResult = ReadResult
    { readresMessages :: [MsgWrapper]
    }
    deriving(Show, Read, Eq)

-- | Wrapper around Msg. This is mostly here so Aeson (un)marshals
-- things correctly; it would be nice to achieve that without
-- this being in the API.
newtype MsgWrapper = MsgWrapper
    { msgwrapperMsg :: Msg
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
    , msgContent        :: MsgContent
    }
    deriving(Show, Read, Eq)

data MsgContent
    = MsgText { msgctText :: ContentText }
    | MsgSystem { msgctSystem :: ContentSystem }
    | MsgAttachment { msgctAttachment :: ContentAttachment }
    | MsgUnfurl { msgctUnfurl :: ContentUnfurl }
    | MsgReaction { msgctReaction :: ContentReaction }
    | MsgDelete { msgctDelete :: ContentDelete }
    | MsgHeadline { msgctHeadline :: ContentHeadline }
    | MsgMetadata { msgctMetadata :: ContentMetadata }
    deriving(Show, Read, Eq)

newtype ContentText = ContentText
    { ctxtBody :: Text
    }
    deriving(Show, Read, Eq)

newtype ContentSystem = ContentSystem
    { ctsysSystemType :: Int
    }
    deriving(Show, Read, Eq)

newtype ContentAttachment = ContentAttachment
    { ctattachUploaded :: Bool
    }
    deriving(Show, Read, Eq)

newtype ContentUnfurl = ContentUnfurl
    { ctunfurlFoo :: Maybe Int
        -- Won't actually appear, just to get this to not fail.
    }
    deriving(Show, Read, Eq)

newtype ContentReaction = ContentReaction
    { ctreactFoo :: Maybe Int
    }
    deriving(Show, Read, Eq)

newtype ContentDelete = ContentDelete
    { ctdelMessageIDs :: [MsgId]
    }
    deriving(Show, Read, Eq)

newtype ContentHeadline = ContentHeadline
    { ctheadHeadline :: Text
    }
    deriving(Show, Read, Eq)

newtype ContentMetadata = ContentMetadata
    { ctmetaConversationTitle :: Text
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

-- Derive JSON type classes. We use the template haskell support for
-- these instead of Generic deriving so do some non-default things
-- in the derivation. For example, H record field names can't overlap,
-- but some of the API's ' do. Similarly, some of our variant types
-- need disambiguating. the mangle* functions help with this.
--
-- See the Aeson docs for more info on how this machinery works.
do  let drv =
            A.deriveJSON A.defaultOptions
                { A.fieldLabelModifier = mangleFieldLabel
                , A.constructorTagModifier = mangleCtorTag
                , A.omitNothingFields = True
                , A.sumEncoding = A.TaggedObject
                    { A.tagFieldName = "type"
                    -- contentsFieldName is unused; we make sure all of
                    -- our sums have record arguments, so they get those field
                    -- names instead:
                    , A.contentsFieldName = "content"
                    }
                }

        mangleCtorTag =
            toSnakeCase
            >>> dropPrefix

        mangleFieldLabel = \case
            -- Most of the fields in the keybase API are snake case,
            -- but a couple of them are camelCase:
            "ctsysSystemType" -> "systemType"
            "ctdelMessageIDs" -> "messageIDs"
            "ctmetaConversationTitle" -> "conversationTitle"
            lbl -> dropPrefix $ toSnakeCase lbl

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
        [ ''Channel
        , ''ContentAttachment
        , ''ContentDelete
        , ''ContentHeadline
        , ''ContentMetadata
        , ''ContentReaction
        , ''ContentSystem
        , ''ContentText
        , ''ContentUnfurl
        , ''Conversation
        , ''ListResult
        , ''Msg
        , ''MsgContent
        , ''MsgWrapper
        , ''ReadResult
        , ''Sender
        ]
