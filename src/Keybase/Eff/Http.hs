module Keybase.Eff.Http
  ( HttpEffect
  , runHttp
  ) where

import Keybase.Prelude

import Control.Effect
import Control.Effect.Carrier
import Control.Effect.Error
import Control.Effect.Interpret
import Network.HTTP.Client
import Network.HTTP.Client.TLS (getGlobalManager)
import UnliftIO.Exception (try)


data HttpEffect (m :: Type -> Type) (k :: Type) where
  SendHttpRequest
    :: Request
    -> (Either HttpException (Response LazyByteString) -> m k)
    -> HttpEffect m k

-- | Send an HTTP request and receive the response. 'Nothing' indicates network
-- flakiness. All other exceptions, most of which indicate the server is
-- misbehaving, are thrown in the 'Error' effect.
sendHttpRequest
  :: forall sig m.
     ( Carrier sig m
     , Member (Error HttpException) sig
     , Member HttpEffect sig
     )
  => Request
  -> m (Maybe (Response LazyByteString))
sendHttpRequest request =
  send (SendHttpRequest request f)
  where
    f
      :: Either HttpException (Response LazyByteString)
      -> m (Maybe (Response LazyByteString))
    f = \case
      Left ex ->
        case ex of
          -- These HTTP exceptions seem normal and all indicate a flakey
          -- connection, translate them all to Nothing.
          HttpExceptionRequest _ ResponseTimeout -> pure Nothing
          HttpExceptionRequest _ ConnectionTimeout -> pure Nothing
          HttpExceptionRequest _ (ConnectionFailure _) -> pure Nothing

          -- Everything else looks crazy and should never occur, throw them in
          -- the Error effect.
          _ -> throwError ex


      Right response ->
        pure (Just response)

-- | Discharge the 'HttpEffect' using the convenient global TLS manager.
runHttp
  :: MonadIO m
  => InterpretC HttpEffect m a
  -> m a
runHttp =
  runInterpret \case
    SendHttpRequest request next -> do
      manager :: Manager <-
        liftIO getGlobalManager

      response :: Either HttpException (Response LazyByteString) <-
        liftIO (try (httpLbs (setRequestIgnoreStatus request) manager))

      next response
