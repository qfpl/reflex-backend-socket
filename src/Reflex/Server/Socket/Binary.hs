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
module Reflex.Server.Socket.Binary (
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
import qualified Data.ByteString.Lazy as LB

import Data.Binary
import Data.Binary.Get

import Reflex

data SocketConfig t a =
  SocketConfig {
    _siMaxRx  :: Int
  , _siOpen   :: Event t Socket
  , _siSend   :: Event t [a]
  , _siClose  :: Event t ()
  }

data SocketOut t b =
  SocketOut {
    _soRecieve :: Event t b
  , _soError   :: Event t String
  , _soClosed  :: Event t ()
  }

socket ::
  forall t m a b.
  ( Reflex t
  , PerformEvent t m
  , TriggerEvent t m
  , MonadIO (Performable m)
  , MonadIO m
  , Binary a
  , Binary b
  ) =>
  SocketConfig t a ->
  m (SocketOut t b)
socket (SocketConfig mxRx eOpen eTx eClose) = mdo
  (eRx, onRx) <- newTriggerEvent
  (eError, onError) <- newTriggerEvent
  (eClosed, onClosed) <- newTriggerEvent

  isOpenRead <- liftIO . atomically $ newEmptyTMVar
  performEvent_ $ ffor eOpen $
    void . liftIO . atomically . tryPutTMVar isOpenRead

  isOpenWrite <- liftIO . atomically $ newEmptyTMVar
  performEvent_ $ ffor eOpen $
    void . liftIO . atomically . tryPutTMVar isOpenWrite

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
          (sendAll sock (LB.toStrict . encode $ bs) >> pure True) `catch`
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

  performEvent_ $ txLoop <$ eOpen

  let
    exHandlerRx :: IOException -> IO B.ByteString
    exHandlerRx e = do
      mSock <- atomically . tryReadTMVar $ isOpenRead
      forM_ mSock $ \_ -> onError (displayException e)
      pure B.empty

    handleDecoding :: Decoder b -> IO (Maybe (Decoder b))
    handleDecoding (Fail _ _ s) = do
      onError s
      pure Nothing
    handleDecoding (Partial f) =
      pure . Just $ Partial f
    handleDecoding (Done bs _ a) = do
      onRx a
      let decoder = runGetIncremental (get :: Get b)
      if B.null bs
      then pure . Just $ decoder
      else handleDecoding $ pushChunk decoder bs

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
          mDecoder <- handleDecoding $ pushChunk decoder bs
          case mDecoder of
            Nothing -> shutdownRx
            Just decoder' -> rxLoop decoder'

    startRxLoop = liftIO $ do
      mSock <- atomically $ tryReadTMVar isOpenRead
      forM_ mSock $ \_ -> void . forkIO $ rxLoop (runGetIncremental (get :: Get b))

  performEvent_ $ startRxLoop <$ eOpen

  let
    closeFn = liftIO $ do
      mSock <- atomically . tryReadTMVar $ isOpenWrite
      forM_ mSock $ \sock -> do
        void . atomically . tryTakeTMVar $ isOpenWrite
        void . atomically . tryTakeTMVar $ isOpenRead
        close sock

  performEvent_ $ closeFn <$ eClose

  pure $ SocketOut eRx eError eClosed
