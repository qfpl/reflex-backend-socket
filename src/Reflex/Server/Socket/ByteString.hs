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
import Data.Foldable (forM_)
import Data.Maybe (isJust, isNothing)

import Control.Exception (IOException, catch, displayException)
import Control.Monad.Trans (MonadIO(..))

import Control.Monad.STM
import Control.Concurrent.STM.TMVar

import Network.Socket hiding (socket, send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Reflex

-- do we want an Event in SocketOut for when the send completes?
data SocketConfig t =
  SocketConfig {
    _siMaxRx  :: Int
  , _siOpen   :: Event t Socket
  , _siSend   :: Event t B.ByteString
  , _siClose  :: Event t ()
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
  , TriggerEvent t m
  , MonadIO (Performable m)
  , MonadIO m
  ) =>
  SocketConfig t ->
  m (SocketOut t)
socket (SocketConfig mxRx eOpen eTx eClose) = mdo
  (eRx, onRx) <- newTriggerEvent
  (eError, onError) <- newTriggerEvent
  (eClosed, onClosed) <- newTriggerEvent

  isOpenRead <- liftIO . atomically $ newEmptyTMVar
  isOpenWrite <- liftIO . atomically $ newEmptyTMVar

  performEvent_ $ (void . liftIO . atomically . tryPutTMVar isOpenRead) <$> eOpen
  performEvent_ $ (void . liftIO . atomically . tryPutTMVar isOpenWrite) <$> eOpen

  let
    exHandlerTx :: IOException -> IO ()
    exHandlerTx e = do
      void . atomically . tryTakeTMVar $ isOpenWrite
      onError (displayException e)

  let
    sendFn bs = liftIO $ do
      mSock <- atomically . tryReadTMVar $ isOpenWrite
      forM_ mSock $ \sock ->
        sendAll sock bs `catch` exHandlerTx

  performEvent_ $ sendFn <$> eTx

  let
    closeFn = liftIO $ do
      mSock <- atomically . tryReadTMVar $ isOpenWrite
      forM_ mSock $ \sock -> do
        close sock
        void . atomically . tryTakeTMVar $ isOpenWrite

  eCloseDone <- performEvent $ closeFn <$ eClose

  let
    exHandlerRx :: IOException -> IO B.ByteString
    exHandlerRx e = do
      onError (displayException e)
      pure B.empty

    -- TODO rework this so that it stops looping if mSock goes from non-nothing to nothing
    -- better yet, start the loop when this thing opens up
    loop = do
      mSock <- atomically $ tryReadTMVar isOpenRead
      forM_ mSock $ \sock -> do
        bs <- recv sock mxRx `catch` exHandlerRx

        -- this may be the thing that we need to do
        -- TODO should we pass these back to the main thread via a queue and call the handlers from there?
        if B.null bs
        then do
          void . atomically $ tryTakeTMVar isOpenRead
          onClosed ()
        else
          onRx bs

      loop

  _ <- liftIO . forkIO $ loop

  pure $ SocketOut eRx eError eClosed
