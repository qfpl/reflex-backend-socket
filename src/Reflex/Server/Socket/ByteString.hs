{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
module Reflex.Server.Socket.ByteString (
    SocketConfig(..)
  , SocketOut(..)
  , socket
  ) where

import Control.Concurrent (forkIO)
import Control.Monad (unless, when, forever, void)
import Data.Foldable (forM_, traverse_)
import Data.Maybe (isJust)

import Control.Exception (IOException, catch, displayException)
import Control.Monad.Trans (MonadIO(..))

import Control.Monad.STM
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TQueue

import Network.Socket hiding (socket, send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

import qualified Data.ByteString as B

import Reflex

data SocketConfig t =
  SocketConfig {
    _siInitSocket :: Socket
  , _siMaxRx      :: Int
  , _siSend       :: Event t [B.ByteString]
  , _siClose      :: Event t ()
  }

data SocketOut t =
  SocketOut {
    _soRecieve :: Event t B.ByteString
  , _soError   :: Event t String
  , _soClosed  :: Event t ()
  }

socket ::
  ( Reflex t
  , PerformEvent t m
  , PostBuild t m
  , TriggerEvent t m
  , MonadIO (Performable m)
  , MonadIO m
  ) =>
  SocketConfig t ->
  m (SocketOut t)
socket (SocketConfig initSock mxRx eTx eClose) = mdo
  (eRx, onRx) <- newTriggerEvent
  (eError, onError) <- newTriggerEvent
  (eClosed, onClosed) <- newTriggerEvent

  payloadQueue <- liftIO newTQueueIO
  isOpenRead <- liftIO . atomically $ newEmptyTMVar
  isOpenWrite <- liftIO . atomically $ newEmptyTMVar

  let
    start = liftIO $ do
      atomically . tryPutTMVar isOpenRead $ initSock
      atomically . tryPutTMVar isOpenWrite $ initSock
      pure ()

  ePostBuild <- getPostBuild
  performEvent_ $ start <$ ePostBuild

  let
    exHandlerTx :: IOException -> IO Bool
    exHandlerTx e = do
      mSock <- atomically . tryReadTMVar $ isOpenWrite
      forM_ mSock $ \_ -> onError (displayException e)
      pure False

    txLoop = do
      mSock <- atomically . tryReadTMVar $ isOpenWrite
      forM_ mSock $ \sock -> do
        bs <- atomically . readTQueue $ payloadQueue
        success <-
          (sendAll sock bs >> pure True) `catch` exHandlerTx
        when success txLoop

    startTxLoop = liftIO $ do
      mSock <- atomically $ tryReadTMVar isOpenWrite
      forM_ mSock $ \_ -> void . forkIO $ txLoop

  performEvent_ $ ffor eTx $ \payloads -> liftIO $ forM_ payloads $
    atomically . writeTQueue payloadQueue

  performEvent_ $ startTxLoop <$ ePostBuild

  let
    exHandlerRx :: IOException -> IO B.ByteString
    exHandlerRx e = do
      mSock <- atomically . tryReadTMVar $ isOpenRead
      forM_ mSock $ \_ -> onError (displayException e)
      pure B.empty

    shutdownRx = do
      void . atomically $ tryTakeTMVar isOpenRead
      onClosed ()

    rxLoop = do
      mSock <- atomically $ tryReadTMVar isOpenRead
      forM_ mSock $ \sock -> do
        bs <- recv sock mxRx `catch` exHandlerRx

        if B.null bs
        then shutdownRx
        else do
          onRx bs
          rxLoop

    startRxLoop = liftIO $ do
      mSock <- atomically $ tryReadTMVar isOpenRead
      forM_ mSock $ \_ -> void . forkIO $ rxLoop

  performEvent_ $ startRxLoop <$ ePostBuild

  let
    exHandlerClose :: IOException -> IO ()
    exHandlerClose =
      onError . displayException

    closeFn = liftIO $ do
      mSock <- atomically . tryReadTMVar $ isOpenWrite
      forM_ mSock $ \sock -> do
        void . atomically . tryTakeTMVar $ isOpenWrite
        void . atomically . tryTakeTMVar $ isOpenRead
        close sock `catch` exHandlerClose

  performEvent_ $ closeFn <$ eClose

  pure $ SocketOut eRx eError eClosed
