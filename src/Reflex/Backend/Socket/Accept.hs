{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TemplateHaskell  #-}

{-|
Copyright   : (c) 2018-2019, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}

module Reflex.Backend.Socket.Accept
  ( accept

    -- * Listen Socket Configuration
  , AcceptConfig(..)

    -- * Output Events
  , Accept(..)

    -- * Lenses
    -- ** AcceptConfig
  , acHostname
  , acService
  , acListenQueue
  , acClose

    -- ** Accept
  , aAcceptSocket
  , aClosed
  , aError
  ) where

import           Control.Concurrent (forkIO)
import           Control.Concurrent.STM (newTMVarIO, tryReadTMVar, tryTakeTMVar)
import           Control.Exception (IOException, catch, onException, try)
import           Control.Monad.Except (ExceptT(..), runExceptT, withExceptT)
import           Control.Monad.STM (atomically)
import           Control.Monad.Trans (MonadIO(..))
import           Data.Foldable (traverse_)
import           Data.Functor (($>), void)
import           Data.List.NonEmpty (NonEmpty, fromList)
import           Data.Semigroup (Last(..))
import           Data.Semigroup.Foldable (asum1)
import           Lens.Micro.TH (makeLenses)
import           Network.Socket (AddrInfo(..), AddrInfoFlag(..), Socket)
import qualified Network.Socket as NS
import           Reflex

-- | Configuration of a listen socket.
data AcceptConfig t = AcceptConfig
  { _acHostname :: Maybe NS.HostName
    -- ^ The hostname to bind to. This will often be 'Nothing', to
    -- bind all interfaces.
  , _acService :: Maybe NS.ServiceName
    -- ^ The port number or service name to listen on. See the
    -- <https://linux.die.net/man/3/getaddrinfo manpage for getaddrinfo>.
  , _acListenQueue :: Int
    -- ^ The length of the "pending connections" queue. See the
    -- <https://linux.die.net/man/2/listen manpage for listen>.
  , _acClose :: Event t ()
    -- ^ Close the listen socket when this event fires. If you plan to
    -- run forever, use 'never'.
  }

$(makeLenses ''AcceptConfig)

-- | Events produced by a running listen socket.
data Accept t = Accept
  { _aAcceptSocket :: Event t (Socket, NS.SockAddr)
    -- ^ A new connection was accepted, including its remote address.
  , _aClosed :: Event t ()
    -- ^ The socket has closed. This will fire exactly once when the
    -- socket closes for any reason, including if your '_acClose'
    -- event fires or if the socket closes in response to a caught
    -- exception caught.
  , _aError :: Event t IOException
    -- ^ An exception occurred. Treat the socket as closed after you
    -- see this. If the socket was open, you will see the
    -- '_aListenClosed' event fire as well, but not necessarily in the
    -- same frame.
  }

$(makeLenses ''Accept)

-- | Create a listening socket. Sockets are accepted in a background
-- thread.
accept
  :: ( Reflex t
     , PerformEvent t m
     , PostBuild t m
     , TriggerEvent t m
     , MonadIO (Performable m)
     , MonadIO m
     )
  => AcceptConfig t
  -> m (Accept t)
accept (AcceptConfig mHost mService listenQueue eClose) = do
  (eAccept, onAccept) <- newTriggerEvent
  (eClosed, onClosed) <- newTriggerEvent
  (eError, onError) <- newTriggerEvent

  isOpen <- liftIO $ newTMVarIO ()

  ePostBuild <- getPostBuild

  let
    start = liftIO $ do
      makeListenSocket >>= \case
        Left exc -> onError exc
        Right sock -> void . forkIO $ acceptLoop sock

      where
        makeListenSocket =
          let
            getAddrs :: ExceptT IOException IO (NonEmpty AddrInfo)
            getAddrs = ExceptT . try $
              -- fromList is probably OK here, as getaddrinfo(3) is required
              -- to return a nonempty list of addrinfos.
              --
              -- See: http://pubs.opengroup.org/onlinepubs/9699919799/functions/getaddrinfo.html
              -- And: https://github.com/haskell/network/issues/407
              fromList <$> NS.getAddrInfo (Just passiveHints) mHost mService
              where passiveHints = NS.defaultHints { addrFlags = [AI_PASSIVE] }

            tryListen :: AddrInfo -> ExceptT IOException IO Socket
            tryListen info = ExceptT . try $ do
              sock <- NS.socket (addrFamily info) NS.Stream NS.defaultProtocol
              (`onException` NS.close sock) $ do
                NS.setSocketOption sock NS.ReuseAddr 1
                NS.bind sock (addrAddress info)
                NS.listen sock listenQueue
              pure sock

          in runExceptT $ do
            addrs <- getAddrs
            let attempts = withExceptT Last . tryListen <$> addrs
            withExceptT getLast $ asum1 attempts

        acceptLoop sock =
          let
            exHandlerAccept :: IOException -> IO (Maybe (Socket, NS.SockAddr))
            exHandlerAccept e = do
              void . atomically $ tryTakeTMVar isOpen
              Nothing <$ onError e
          in do
            atomically (tryReadTMVar isOpen) >>= \case
              Nothing -> do
                NS.close sock
                onClosed ()
              Just () -> do
                (Just <$> NS.accept sock) `catch` exHandlerAccept
                  >>= traverse_ onAccept
                acceptLoop sock

  performEvent_ $ ePostBuild $> start

  performEvent_ $ eClose $> liftIO (void . atomically $ tryTakeTMVar isOpen)

  pure $ Accept eAccept eClosed eError
