module Main (main) where

import Zhp

import Api.Types

import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as LBS

main :: IO ()
main =  pure ()
{-
 -- FIXME: for some reason, the compiler can't find the A.FromJSON instance
 -- for ListResult. I(zenhack) am going to do some work on the UI and then
 -- come back to debug this.
main = do
    resultBytes <- LBS.readFile "testdata/list-result.json"
    print (A.decode resultBytes :: Maybe ListResult)
-}
