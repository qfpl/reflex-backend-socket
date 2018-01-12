{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE FlexibleContexts #-}
module Reflex.Server.Socket.Connect (
    ConnectConfig(..)
  , Connect(..)
  , connect
  ) where

import Control.Monad (forM_)

import Control.Exception (IOException, catch, displayException)
import Control.Monad.Trans (MonadIO(..))

import Network.Socket hiding (connect, accept)
import qualified Network.Socket as NS

import Reflex

data ConnectConfig =
  ConnectConfig {
    _ccHostname :: Maybe String
  , _ccPort     :: Maybe String
  }

data Connect t =
  Connect {
    _cSocket :: Event t Socket
  , _cError  :: Event t String
  }

connect ::
  ( Reflex t
  , PerformEvent t m
  , TriggerEvent t m
  , PostBuild t m
  , MonadIO (Performable m)
  , MonadIO m
  ) =>
  ConnectConfig ->
  m (Connect t)
connect (ConnectConfig mHost mPort) = do
  (eSocket, onSocket) <- newTriggerEvent
  (eError, onError)   <- newTriggerEvent

  let
    exHandlerGetAddr :: IOException -> IO [AddrInfo]
    exHandlerGetAddr e = do
      onError . displayException $ e
      pure []

    getAddr :: IO (Maybe AddrInfo)
    getAddr = do
      addrInfos <- getAddrInfo Nothing mHost mPort `catch` exHandlerGetAddr
      pure $ case addrInfos of
        [] -> Nothing
        h : _ -> Just h

    exHandler :: IOException -> IO ()
    exHandler =
      onError . displayException

    connectAddr :: AddrInfo -> IO ()
    connectAddr h = do
      sock <- socket (addrFamily h) Stream defaultProtocol
      NS.connect sock (addrAddress h)
      onSocket sock

    start = liftIO $ do
      mAddrInfo <- getAddr
      forM_ mAddrInfo $ \h ->
        connectAddr h `catch` exHandler

  ePostBuild <- getPostBuild
  performEvent_ $ start <$ ePostBuild

  pure $ Connect eSocket eError
