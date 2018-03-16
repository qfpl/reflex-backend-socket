{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module Reflex.Server.Socket.Connect (
    ConnectConfig(..)
  , ccHostname
  , ccPort
  , Connect(..)
  , cSocket
  , cError
  , connect
  ) where

import Control.Monad (forM_, forM)

import Control.Exception (IOException, catch, displayException)
import Control.Monad.Trans (MonadIO(..))

import Control.Lens

import Network.Socket hiding (connect)
import qualified Network.Socket as NS

import Reflex

data ConnectConfig =
  ConnectConfig {
    _ccHostname :: Maybe String
  , _ccPort     :: Maybe String
  }

makeLenses ''ConnectConfig

data Connect t =
  Connect {
    _cSocket :: Event t Socket
  , _cError  :: Event t String
  }

makeLenses ''Connect

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

    exHandler :: IOException -> IO (Maybe Socket)
    exHandler e = do
      onError . displayException $ e
      pure Nothing

    connectAddr :: AddrInfo -> IO (Maybe Socket)
    connectAddr h = do
      sock <- socket (addrFamily h) Stream defaultProtocol
      NS.connect sock (addrAddress h)
      pure (Just sock)

    start = liftIO $ do
      mAddrInfo <- getAddr
      forM_ mAddrInfo $ \h -> do
        mSock <- connectAddr h `catch` exHandler
        forM mSock onSocket

  ePostBuild <- getPostBuild
  performEvent_ $ start <$ ePostBuild

  pure $ Connect eSocket eError
