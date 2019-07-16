module Keybase.Eff.Http
  ( HttpEffect
  , sendHttpRequest
  , runHttp
  , FlakeyNetwork(..)
  ) where

import Keybase.Prelude

import Control.Effect
import Control.Effect.Carrier
import Control.Effect.Error
import Control.Effect.Interpret
import Network.HTTP.Client
import Network.HTTP.Client.TLS (getGlobalManager)
import UnliftIO.Exception (try)


data FlakeyNetwork
  = FlakeyNetwork
  deriving stock (Show)

data HttpEffect (m :: Type -> Type) (k :: Type) where
  SendHttpRequest
    :: Request
    -> (Either HttpException (Response LazyByteString) -> m k)
    -> HttpEffect m k
  deriving stock (Functor, Generic1)
  deriving anyclass (HFunctor)

-- | Send an HTTP request and receive the response.
sendHttpRequest
  :: forall sig m.
     ( Carrier sig m
     , Member (Error FlakeyNetwork) sig
     , Member (Error HttpException) sig
     , Member HttpEffect sig
     )
  => Request
  -> m (Response LazyByteString)
sendHttpRequest request =
  send (SendHttpRequest request (either handleException pure))
  where
    handleException :: HttpException -> m a
    handleException = \case
      HttpExceptionRequest _ ResponseTimeout -> throwError FlakeyNetwork
      HttpExceptionRequest _ ConnectionTimeout -> throwError FlakeyNetwork
      HttpExceptionRequest _ (ConnectionFailure _) -> throwError FlakeyNetwork
      ex -> throwError ex


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
