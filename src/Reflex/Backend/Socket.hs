{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module Reflex.Backend.Socket (
    SocketConfig(..)
  , scInitSocket
  , scMaxRx
  , scSend
  , scClose
  , Socket(..)
  , sReceive
  , sOpen
  , sError
  , sClosed
  , socket
  , module Reflex.Backend.Socket.Connect
  , module Reflex.Backend.Socket.Accept
  ) where

import Control.Concurrent (forkIO)
import Control.Monad (when, void)
import Data.Foldable (forM_)

import Control.Exception (IOException, catch)

import Control.Monad.Trans (MonadIO(..))

import Control.Monad.STM
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TQueue

import Control.Lens

import qualified Data.ByteString as B

import Network.Socket hiding (Socket, socket, send, sendTo, recv, recvFrom, accept)
import qualified Network.Socket as NS
import Network.Socket.ByteString

import Reflex

import Reflex.Binary

import Reflex.Backend.Socket.Connect
import Reflex.Backend.Socket.Accept

data SocketConfig t a =
  SocketConfig {
    _scInitSocket :: NS.Socket
  , _scMaxRx      :: Int
  , _scSend       :: Event t [a]
  , _scClose      :: Event t ()
  }

makeLenses ''SocketConfig

data SocketError
  = SocketIOException IOException
  | SocketDecodeError String
  deriving (Eq, Show)

data Socket t b =
  Socket {
    _sReceive :: Event t b
  , _sOpen    :: Event t ()
  , _sError   :: Event t SocketError
  , _sClosed  :: Event t ()
  }

makeLenses ''Socket

socket ::
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
socket (SocketConfig initSock mxRx eTx eClose) = mdo
  (eRx, onRx) <- newTriggerEvent
  (eOpen, onOpen) <- newTriggerEvent
  (eError, onError) <- newTriggerEvent
  (eClosed, onClosed) <- newTriggerEvent

  payloadQueue <- liftIO newTQueueIO
  closeQueue <- liftIO . atomically $ newEmptyTMVar
  isOpenRead <- liftIO . atomically $ newEmptyTMVar
  isOpenWrite <- liftIO . atomically $ newEmptyTMVar

  ePostBuild <- getPostBuild

  let
    exHandlerClose :: IOException -> IO ()
    exHandlerClose = onError . SocketIOException

    exHandlerTx :: IOException -> IO Bool
    exHandlerTx e = do
      mSock <- atomically . tryReadTMVar $ isOpenWrite
      forM_ mSock $ \_ -> onError (SocketIOException e)
      pure False

    txLoop = do
      let
        stmTx = do
          mSock <- tryReadTMVar isOpenWrite
          case mSock of
            Nothing -> pure (Left Nothing)
            Just sock -> do
              bs <- readTQueue payloadQueue
              pure $ Right (sock, bs)
        stmClose = do
          mSock <- tryReadTMVar isOpenWrite
          case mSock of
            Nothing -> pure (Left Nothing)
            Just sock -> do
              _ <- takeTMVar closeQueue
              pure (Left (Just sock))
      e <- atomically $ stmClose `orElse` stmTx
      case e of
        Right (sock, bs) -> do
          success <- (sendAll sock (doEncode bs) >> pure True) `catch` exHandlerTx
          when success txLoop
        Left (Just sock) -> do
          void . atomically . tryTakeTMVar $ isOpenWrite
          void . atomically . tryTakeTMVar $ isOpenRead
          close sock `catch` exHandlerClose
          onClosed ()
        Left Nothing ->
          txLoop

    startTxLoop = liftIO $ do
      mSock <- atomically $ tryReadTMVar isOpenWrite
      forM_ mSock $ \_ -> void . forkIO $ txLoop


  let
    exHandlerRx :: IOException -> IO B.ByteString
    exHandlerRx e = do
      mSock <- atomically . tryReadTMVar $ isOpenRead
      forM_ mSock $ \_ -> onError (SocketIOException e)
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
          else
            runIncrementalDecoder
              (onError . SocketDecodeError)
              onRx
              (const shutdownRx)
              rxLoop
              decoder
              bs

    startRxLoop = liftIO $ do
      mSock <- atomically $ tryReadTMVar isOpenRead
      forM_ mSock $ const . void . forkIO . rxLoop $ getDecoder

  performEvent_ $ ffor eTx $ \payloads -> liftIO $ forM_ payloads $
    atomically . writeTQueue payloadQueue

  performEvent_ $ ffor eClose $ \_ ->
    liftIO . atomically . putTMVar closeQueue $ ()

  let
    start = liftIO $ do
      void . atomically . tryPutTMVar isOpenRead $ initSock
      void . atomically . tryPutTMVar isOpenWrite $ initSock
      startTxLoop
      startRxLoop
      onOpen ()
      pure ()

  performEvent_ $ start <$ ePostBuild

  pure $ Socket eRx eOpen eError eClosed
