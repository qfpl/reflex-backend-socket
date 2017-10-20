{-|
Copyright   : (c) 2007, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE FlexibleContexts #-}
module Reflex.Server.Socket (
    ConnectConfig(..)
  , Connect(..)
  , connect
  , AcceptConfig(..)
  , Accept(..)
  , accept
  ) where

import Control.Concurrent (forkIO)
import Control.Monad (forever, void, forM_)

import Control.Exception (IOException, catch, displayException)
import Control.Monad.Trans (MonadIO(..))

import Control.Monad.STM
import Control.Concurrent.STM.TMVar

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

data AcceptConfig t =
  AcceptConfig {
    _acHostname    :: Maybe String
  , _acPort        :: Maybe String
  , _acListenQueue :: Int
  , _acClose       :: Event t ()
  }

data Accept t =
  Accept {
    _aAcceptSocket :: Event t (Socket, SockAddr)
  , _aListenClosed :: Event t ()
  , _aError        :: Event t String
  }

accept ::
  ( Reflex t
  , PerformEvent t m
  , PostBuild t m
  , TriggerEvent t m
  , MonadIO (Performable m)
  , MonadIO m
  ) =>
  AcceptConfig t ->
  m (Accept t)
accept (AcceptConfig mHost mPort listenQueue eClose) = do
  (eAcceptSocket, onAcceptSocket) <- newTriggerEvent
  (eClosed, onClosed) <- newTriggerEvent
  (eError, onError) <- newTriggerEvent

  isOpen <- liftIO . atomically $ newEmptyTMVar

  let
    exHandlerGetAddr :: IOException -> IO [AddrInfo]
    exHandlerGetAddr e = do
      onError . displayException $ e
      pure []

    getAddr :: IO (Maybe AddrInfo)
    getAddr = do
      addrInfos <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) mHost mPort `catch` exHandlerGetAddr
      pure $ case addrInfos of
        [] -> Nothing
        h : _ -> Just h

    exHandlerSocket :: IOException -> IO (Maybe Socket)
    exHandlerSocket e = do
      onError . displayException $ e
      pure Nothing

    listenAddr :: AddrInfo -> IO (Maybe Socket)
    listenAddr h = do
      sock <- socket (addrFamily h) Stream defaultProtocol
      bind sock (addrAddress h)
      listen sock listenQueue
      pure $ Just sock

    exHandlerAccept :: IOException -> IO (Maybe (Socket, SockAddr))
    exHandlerAccept e = do
      void . atomically . tryTakeTMVar $ isOpen
      onError . displayException $ e
      pure Nothing

    acceptLoop :: IO ()
    acceptLoop = do
      mSock <- atomically . tryReadTMVar $ isOpen
      forM_ mSock $ \sock ->  do
        mConn <- (Just <$> NS.accept sock) `catch` exHandlerAccept
        forM_ mConn $ \conn -> do
          onAcceptSocket conn
          acceptLoop

    start = liftIO $ do
      mAddrInfos <- getAddr
      case mAddrInfos of
        Nothing -> pure ()
        Just h -> do
          mSocket <- listenAddr h `catch` exHandlerSocket
          case mSocket of
            Nothing -> pure ()
            Just sock -> do
              void . atomically . tryPutTMVar isOpen $ sock
              void . forkIO $ acceptLoop

    closeSock = liftIO $ do
      mSock <- atomically . tryTakeTMVar $ isOpen
      forM_ mSock close

  ePostBuild <- getPostBuild
  performEvent_ $ start <$ ePostBuild

  performEvent_ $ ffor eClose $ const closeSock

  pure $ Accept eAcceptSocket eClosed eError
