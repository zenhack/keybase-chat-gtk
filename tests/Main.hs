module Main (main) where

import Zhp

import Api.Types

import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as LBS

main :: IO ()
main = do
    resultBytes <- LBS.readFile "testdata/list-result.json"
    print (A.decode resultBytes :: Maybe ListResult)
