module Keybase.Prelude
  ( LazyByteString
  , module X
  ) where

import Data.ByteString as X (ByteString)
import Data.Coerce as X (coerce)
import Data.Kind as X (Type)
import Data.Text as X (Text)
import GHC.Generics as X (Generic, Generic1)
import Zhp as X

import qualified Data.ByteString.Lazy

type LazyByteString
  = Data.ByteString.Lazy.ByteString
