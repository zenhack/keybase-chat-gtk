module Keybase where

import Keybase.Eff.Http
import Keybase.Prelude

import Control.Effect
import Control.Effect.Error
import Data.Scientific (floatingOrInteger)
import Network.HTTP.Client
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text.Encoding as Text


newtype CsrfToken
  = CsrfToken { unCsrfToken :: Text }
  deriving stock (Show)

newtype EmailAddress
  = EmailAddress { unEmailAddress :: Text }
  deriving stock (Show)

newtype Hexadecimal a
  = Hexadecimal { unHexadecimal :: a }
  deriving stock (Show)

-- | Something went wrong: either a generic non-200 response code, or some
-- custom error response.
data KeybaseError
  = KeybaseError Request (Response LazyByteString)
  deriving stock (Show)

newtype Kid
  = Kid { unKid :: Text }
  deriving stock (Show)

newtype LoginSession
  = LoginSession { unLoginSession :: Text }
  deriving stock (Show)

newtype Password
  = Password { unPassword :: Text }
  deriving stock (Show)

newtype PwhVersion
  = PwhVersion { unPwhVersion :: Int }
  deriving stock (Show)

newtype Salt
  = Salt { unSalt :: Hexadecimal Text }
  deriving stock (Show)

newtype SessionCookie
  = SessionCookie { unSessionCookie :: Text }
  deriving stock (Show)

newtype Username
  = Username { unUsername :: Text }
  deriving stock (Show)

newtype Uid
  = Uid { unUid :: Hexadecimal Text }
  deriving stock (Show)

type User
  = Aeson.Value

defaultKeybaseRequest :: Request
defaultKeybaseRequest =
  defaultRequest
    { host = "keybase.io"
    , port = 443
    , secure = True
    }

-- | https://keybase.io/docs/api/1.0/call/getsalt
--
-- @
-- :l Keybase
-- getsalt (Right (Username "mitchellsalad")) & runHttp & runError & runError & runError & runM :: IO (Either FlakeyNetwork (Either HttpException (Either KeybaseError (CsrfToken, LoginSession, Salt, Uid))))
-- @
getsalt
  :: ( Carrier sig m
     , Member (Error FlakeyNetwork) sig
     , Member (Error HttpException) sig
     , Member (Error KeybaseError) sig
     , Member HttpEffect sig
     , MonadIO m
     )
  => Either EmailAddress Username
  -> m (CsrfToken, LoginSession, Salt, Uid)
getsalt emailAddressOrUsername = do
  response :: Response LazyByteString <-
    sendHttpRequest request

  maybe (throwError (KeybaseError request response)) pure do
    guard (responseStatus response == status200)

    Aeson.Object object <-
      Aeson.decode (responseBody response)

    Aeson.Object statusObject <-
      HashMap.lookup "status" object

    Aeson.Number 0 <-
      HashMap.lookup "code" statusObject

    Aeson.String csrfToken <-
      HashMap.lookup "csrf_token" object

    Aeson.String loginSession <-
      HashMap.lookup "login_session" object

    Aeson.Number (floatingOrInteger -> Right pwhVersion) <-
      HashMap.lookup "pwh_version" object

    Aeson.String salt <-
      HashMap.lookup "salt" object

    Aeson.String uid <-
      HashMap.lookup "uid" object

    pure
      ( CsrfToken csrfToken
      , LoginSession loginSession
      , Salt (Hexadecimal salt)
      , Uid (Hexadecimal uid)
      )

  where
    slug :: ByteString
    slug =
      Text.encodeUtf8 (either id id (coerce emailAddressOrUsername))

    request :: Request
    request =
      defaultKeybaseRequest
        { method = methodGet
        , path = "/_/api/1.0/getsalt.json"
        }
        & setQueryString [("email_or_username", Just slug)]

-- | https://keybase.io/docs/api/1.0/call/login
login
  :: ( Carrier sig m
     , Member HttpEffect sig
     )
  => Either EmailAddress Username
  -> Password
  -> Salt -- ^ from 'getsalt'
  -> Kid
  -> m (SessionCookie, User)
login =
  undefined
