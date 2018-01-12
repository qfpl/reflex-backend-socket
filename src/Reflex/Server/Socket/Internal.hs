{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Reflex.Server.Socket.Internal (
    mkSocket
  ) where

import Control.Concurrent (forkIO)
import Control.Monad (when, void)
import Data.Foldable (forM_)

import Control.Exception (IOException, catch, displayException)

import Control.Monad.Trans (MonadIO(..))

import Control.Monad.STM
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TQueue

import qualified Data.ByteString as B

import Network.Socket hiding (Socket, socket, mkSocket, send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

import Reflex

import Reflex.Binary

import Reflex.Server.Socket.Common

mkSocket ::
  forall t m a b.
  ( Reflex t
  , PerformEvent t m
  , PostBuild t m
  , TriggerEvent t m
  , MonadIO (Performable m)
  , MonadIO m
  , CanEncode a
  , CanDecode b
  ) =>
  SocketConfig t a ->
  m (Socket t b)
mkSocket (SocketConfig initSock mxRx eTx eClose) = mdo
  (eRx, onRx) <- newTriggerEvent
  (eOpen, onOpen) <- newTriggerEvent
  (eError, onError) <- newTriggerEvent
  (eClosed, onClosed) <- newTriggerEvent

  payloadQueue <- liftIO newTQueueIO
  isOpenRead <- liftIO . atomically $ newEmptyTMVar
  isOpenWrite <- liftIO . atomically $ newEmptyTMVar

  let
    start = liftIO $ do
      void . atomically . tryPutTMVar isOpenRead $ initSock
      void . atomically . tryPutTMVar isOpenWrite $ initSock
      onOpen ()
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
          (sendAll sock (doEncode bs) >> pure True) `catch` exHandlerTx
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

    rxLoop decoder = do
      mSock <- atomically $ tryReadTMVar isOpenRead
      forM_ mSock $ \sock -> do
        bs <- recv sock mxRx `catch` exHandlerRx

        if B.null bs
        then shutdownRx
        else do
          case decoder of
            IncrementalDecoder c stepRx -> do
              mDecoder <- stepRx onError onRx bs c
              case mDecoder of
                Nothing -> shutdownRx
                Just c' -> rxLoop $ IncrementalDecoder c' stepRx

    startRxLoop = liftIO $ do
      mSock <- atomically $ tryReadTMVar isOpenRead
      forM_ mSock $ const . void . forkIO . rxLoop $ getDecoder

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
        onClosed ()

  performEvent_ $ closeFn <$ eClose

  pure $ Socket eRx eOpen eError eClosed
