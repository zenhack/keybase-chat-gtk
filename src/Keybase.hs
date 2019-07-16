module Keybase where

import Zhp
import Data.Text (Text)

import qualified Data.Aeson as Aeson


newtype CsrfToken
  = CsrfToken { unCsrfToken :: Text }

newtype EmailAddress
  = EmailAddress { unEmailAddress :: Text }

newtype Hexadecimal a
  = Hexadecimal { unHexadecimal :: a }

newtype Kid
  = Kid { unKid :: Text }

newtype LoginSession
  = LoginSession { unLoginSession :: Text }

newtype Password
  = Password { unPassword :: Text }

newtype Salt
  = Salt { unSalt :: Hexadecimal Text }

newtype SessionCookie
  = SessionCookie { unSessionCookie :: Text }

newtype Username
  = Username { unUsername :: Text }

type User
  = Aeson.Value

-- | https://keybase.io/docs/api/1.0/call/getsalt
getsalt
  :: Either EmailAddress Username
  -> IO (Salt, CsrfToken, LoginSession)
getsalt =
  undefined

-- | https://keybase.io/docs/api/1.0/call/login
login
  :: Either EmailAddress Username
  -> Password
  -> Salt -- ^ from 'getsalt'
  -> Kid
  -> IO (SessionCookie, User)
login =
  undefined
