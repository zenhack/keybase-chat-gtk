module Main (main) where

import Zhp

import Test.Hspec

import Api.Types

import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as LBS


result = ListResult
    { listresConversations =
        [ Conversation
            { convId = ConversationId "abcdef01235390abe0320abc02ff2a0b0acef0a32376816ab3298500ca4329af"
            , convChannel = Channel
                { chanName = "crypto_nerds"
                , chanPublic = False
                , chanMembersType = MembersType "team"
                , chanTopicType = TopicType "chat"
                , chanTopicName = Just "general"
                }
            , convUnread = False
            }
        , Conversation
            { convId = ConversationId "3782956167bacde1349fa23c389132e89a3289f93106809cef92039508b3289b"
            , convChannel = Channel
                { chanName = "alice,bob"
                , chanPublic = False
                , chanMembersType = MembersType "impteamnative"
                , chanTopicType = TopicType "chat"
                , chanTopicName = Nothing
                }
            , convUnread = False
            }
        ]
    , listresOffline = False
    }

main :: IO ()
main = hspec $ do
    describe "decoding resutlts" $ do
        it "Should decode the test data correctly" $ do
            resultBytes <- LBS.readFile "testdata/list-result.json"
            A.decode resultBytes `shouldBe` Just result
