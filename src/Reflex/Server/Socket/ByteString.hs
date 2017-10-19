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
import Control.Monad (unless, forever, void)
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

  ePostBuild <- getPostBuild

  isOpenRead <- liftIO . atomically $ newEmptyTMVar
  performEvent_ $ ffor ePostBuild $
    const . void . liftIO . atomically . tryPutTMVar isOpenRead $ initSock

  isOpenWrite <- liftIO . atomically $ newEmptyTMVar
  performEvent_ $ ffor ePostBuild $
    const . void . liftIO . atomically . tryPutTMVar isOpenWrite $ initSock

  payloadQueue <- liftIO newTQueueIO

  let
    exHandlerTx :: IOException -> IO ()
    exHandlerTx e =
      onError (displayException e)

    sendFn bs = do
      mSock <- atomically . tryReadTMVar $ isOpenWrite
      case mSock of
        Nothing -> pure False
        Just sock ->
          (sendAll sock bs >> pure True) `catch`
          (\e -> exHandlerTx e >> pure False)

    txLoop = liftIO . void . forkIO . forever $ do
      bs <- atomically $ do
        pl     <- readTQueue payloadQueue
        open   <- tryReadTMVar isOpenWrite
        if isJust open then return pl else retry
      success <- sendFn bs
      unless success $
        atomically $ unGetTQueue payloadQueue bs

  performEvent_ $ ffor eTx $
    liftIO . atomically . traverse_ (writeTQueue payloadQueue)

  performEvent_ $ txLoop <$ ePostBuild

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
    closeFn = liftIO $ do
      mSock <- atomically . tryReadTMVar $ isOpenWrite
      forM_ mSock $ \sock -> do
        void . atomically . tryTakeTMVar $ isOpenWrite
        void . atomically . tryTakeTMVar $ isOpenRead
        close sock

  performEvent_ $ closeFn <$ eClose

  pure $ SocketOut eRx eError eClosed
