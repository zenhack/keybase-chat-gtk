module Keybase.Prelude
  ( LazyByteString
  , module X
  ) where

import Data.Kind as X (Type)
import Data.Text as X (Text)
import Zhp as X

import qualified Data.ByteString.Lazy

type LazyByteString
  = Data.ByteString.Lazy.ByteString
