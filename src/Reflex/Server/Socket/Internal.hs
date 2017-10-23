{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Reflex.Server.Socket.Internal (
    MkTx(..)
  , MkRx(..)
  , mkSocket
  ) where

import Control.Concurrent (forkIO)
import Control.Monad (unless, when, forever, void)
import Data.Foldable (forM_, traverse_)

import Control.Exception (IOException, catch, displayException)

import Control.Monad.Trans (MonadIO(..))

import Control.Monad.STM
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TQueue

import qualified Data.ByteString as B

import Network.Socket hiding (Socket, socket, mkSocket, send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

import Reflex

import Reflex.Server.Socket.Common

data MkTx a where
  MkTx :: (a -> B.ByteString) -> MkTx a

data MkRx b where
  MkRx :: c -> ((String -> IO ()) -> (b -> IO ()) -> B.ByteString -> c -> IO (Maybe c)) -> MkRx b

mkSocket ::
  forall t m a b.
  ( Reflex t
  , PerformEvent t m
  , PostBuild t m
  , TriggerEvent t m
  , MonadIO (Performable m)
  , MonadIO m
  ) =>
  MkTx a ->
  MkRx b ->
  SocketConfig t a ->
  m (Socket t b)
mkSocket (MkTx encodeTx) (MkRx initRx stepRx) (SocketConfig initSock mxRx eTx eClose) = mdo
  (eRx, onRx) <- newTriggerEvent
  (eOpen, onOpen) <- newTriggerEvent
  (eError, onError) <- newTriggerEvent
  (eClosed, onClosed) <- newTriggerEvent

  payloadQueue <- liftIO newTQueueIO
  isOpenRead <- liftIO . atomically $ newEmptyTMVar
  isOpenWrite <- liftIO . atomically $ newEmptyTMVar

  let
    start = liftIO $ do
      atomically . tryPutTMVar isOpenRead $ initSock
      atomically . tryPutTMVar isOpenWrite $ initSock
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
          (sendAll sock (encodeTx bs) >> pure True) `catch` exHandlerTx
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
          mDecoder <- stepRx onError onRx bs decoder
          case mDecoder of
            Nothing -> shutdownRx
            Just decoder' -> rxLoop decoder'

    startRxLoop = liftIO $ do
      mSock <- atomically $ tryReadTMVar isOpenRead
      forM_ mSock $ \_ -> void . forkIO . rxLoop $ initRx

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
