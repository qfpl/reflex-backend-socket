{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

{-|
Copyright   : (c) 2018-2019, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}

module Reflex.Backend.Socket
  ( socket

    -- * Socket Configuration
  , SocketConfig(..)

    -- * Socket Output Events
  , Socket(..)

    -- * Convenience Re-Exports
  , connect
  , module Reflex.Backend.Socket.Accept

  -- * Lenses
  -- ** SocketConfig
  , scInitSocket
  , scMaxRx
  , scSend
  , scClose

  -- ** Socket
  , sReceive
  , sOpen
  , sClose
  , sError
  ) where

import           Control.Concurrent (forkIO)
import qualified Control.Concurrent.STM as STM
import           Control.Exception (IOException, try)
import           Control.Lens.TH (makeLenses)
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.STM (atomically)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Functor (($>), (<&>), void)
import           Data.Functor.Apply ((<.>))
import           Data.These
import qualified Network.Socket as NS
import           Network.Socket.ByteString (sendAll, recv)
import           Reflex
import           Reflex.Backend.Socket.Accept
import           Reflex.Backend.Socket.Connect (connect)

-- | Holds the socket to wire into the FRP network, and events that
-- drive it.
data SocketConfig t = SocketConfig
  { _scInitSocket :: NS.Socket
    -- ^ Socket to wrap.
  , _scMaxRx :: Int
    -- ^ Maximum number of bytes to read at a time.
  , _scSend :: Event t ByteString
    -- ^ Data to send out on this socket.
  , _scClose :: Event t ()
    -- ^ Ask to close the socket. The socket will stop trying to
    -- receive data (and the '_sReceive' event will stop firing), and
    -- the socket will be "drained": future events on '_scSend' will
    -- be ignored, and it will close after writing all pending data.
    -- If '_scSend' and '_scClose' fire in the same frame, the data
    -- will nevertheless be queued for sending.
  }

$(makeLenses ''SocketConfig)

-- | Events produced by an active socket.
data Socket t = Socket
  { _sReceive :: Event t ByteString
    -- ^ Data has arrived.
  , _sOpen :: Event t ()
    -- ^ The socket has opened, and its receive/send loops are running.
  , _sClose :: Event t ()
    -- ^ The socket has closed. This will fire exactly once when the
    -- socket closes for any reason, including if your '_scClose'
    -- event fires, the other end disconnects, or if the socket closes
    -- in response to a caught exception.
  , _sError :: Event t IOException
    -- ^ An exception occurred. Treat the socket as closed after you
    -- see this. If the socket was open, you will see the '_sClosed'
    -- event fire as well, but not necessarily in the same frame.
  }

$(makeLenses ''Socket)

data SocketState
  = Open
    -- ^ Data flows in both directions
  | Draining
    -- ^ We've been asked to close, but will transmit all pending data
    -- first (and not accept any more)
  | Closed
    -- ^ Hard close. Don't transmit pending data.

-- | Wire a socket into the FRP network. You will likely use this to
-- attach events to a socket that you just connected (from 'connect'),
-- or a socket that you just accepted (from the '_aAcceptSocket' event
-- you got when you called 'accept').
socket
  :: forall t m.
     ( Reflex t
     , PerformEvent t m
     , PostBuild t m
     , TriggerEvent t m
     , MonadIO (Performable m)
     , MonadIO m
     )
  => SocketConfig t
  -> m (Socket t)
socket (SocketConfig sock maxRx eTx eClose) = do
  (eRx, onRx) <- newTriggerEvent
  (eOpen, onOpen) <- newTriggerEvent
  (eClosed, onClosed) <- newTriggerEvent
  (eError, onError) <- newTriggerEvent

  payloadQueue <- liftIO STM.newTQueueIO
  state <- liftIO $ STM.newTVarIO Open

  ePostBuild <- getPostBuild

  let
    start = liftIO $ do
      void $ forkIO txLoop
      void $ forkIO rxLoop
      onOpen ()

      where
        txLoop =
          let
            loop = do
              mBytes <- atomically $
                STM.readTVar state >>= \case
                  Closed -> pure Nothing
                  _ -> STM.tryReadTQueue payloadQueue
                    >>= maybe STM.retry (pure . Just)

              case mBytes of
                Nothing -> onClosed ()
                Just bs -> do
                  try (sendAll sock bs) >>= \case
                    Left exc -> onError exc *> shutdown
                    Right () -> pure ()
                  loop
          in loop

        rxLoop =
          let
            loop = atomically (STM.readTVar state) >>= \case
              Open -> try (recv sock maxRx) >>= \case
                Left exc -> onError exc *> shutdown
                Right bs
                  | B.null bs -> shutdown
                  | otherwise -> onRx bs *> loop
              _ -> pure ()
          in loop

        shutdown = void . atomically $ STM.writeTVar state Closed

  performEvent_ $ ePostBuild $> start

  -- If we see a tx and a close event in the same frame, we want to
  -- process the tx before the close, so it doesn't get lost.
  let
    eTxOrClose :: Event t (These ByteString ())
    eTxOrClose = leftmost
      [ These <$> eTx <.> eClose
      , This <$> eTx
      , That <$> eClose
      ]

    queueSend bs = STM.readTVar state >>= \case
      Open -> STM.writeTQueue payloadQueue bs
      _ -> pure ()
    queueClose = STM.writeTVar state Draining

  performEvent_ $ eTxOrClose <&> \t -> liftIO . atomically $ case t of
    This bs -> queueSend bs
    That () -> queueClose
    These bs () -> queueSend bs *> queueClose

  pure $ Socket eRx eOpen eClosed eError
