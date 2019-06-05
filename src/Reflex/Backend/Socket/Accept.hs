{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TupleSections    #-}

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

    -- * Results of @accept@
  , Accept(..)
  , AcceptError(..)

    -- * Lenses
    -- ** AcceptConfig
  , acHostname
  , acService
  , acListenQueue
  , acSocketOptions
  , acClose

    -- ** Accept
  , aAcceptSocket
  , aClose
  , aError

    -- ** AcceptError
  , _GetAddrInfoError
  , _BindError
  ) where

import           Control.Concurrent (forkIO)
import qualified Control.Concurrent.STM as STM
import           Control.Exception (IOException, onException, try)
import           Control.Lens.TH (makeLenses, makePrisms)
import           Control.Monad.Except (ExceptT(..), runExceptT, withExceptT)
import           Control.Monad.STM (atomically)
import           Control.Monad.Trans (MonadIO(..))
import           Data.Foldable (traverse_)
import           Data.Functor (($>), void)
import           Data.List.NonEmpty (NonEmpty, fromList)
import           Data.Semigroup.Foldable (asum1)
import           GHC.Generics (Generic)
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
  , _acSocketOptions :: [(NS.SocketOption, Int)]
    -- ^ List of socket options, passed one at a time to
    -- 'NS.setSocketOption'. Many people will want
    -- @[('NS.ReuseAddr', 1)]@ here.
  , _acClose :: Event t ()
    -- ^ Close the listen socket when this event fires. If you plan to
    -- run forever, use 'never'.
  }

$(makeLenses ''AcceptConfig)

-- | Events produced by a running listen socket.
data Accept t = Accept
  { _aAcceptSocket :: Event t (Socket, NS.SockAddr)
    -- ^ A new connection was accepted, including its remote address.
  , _aClose :: Event t ()
    -- ^ The socket has closed. This will fire exactly once when the
    -- socket closes for any reason, including if your '_acClose'
    -- event fires or if the socket closes in response to a caught
    -- exception.
  , _aError :: Event t IOException
    -- ^ An exception occurred. Treat the socket as closed after you
    -- see this. You will see the '_aClose' event fire as well, but
    -- not necessarily in the same frame.
  }

$(makeLenses ''Accept)

-- | If 'accept' fails, you'll inspect one of these to find out why.
data AcceptError
  = GetAddrInfoError IOException
    -- ^ Call to 'getAddrInfo' failed.
  | BindError (NonEmpty (AddrInfo, IOException))
    -- ^ We failed to bind any 'AddrInfo' we were given, and here are
    -- the corresponding exceptions each time we tried.
  deriving (Eq, Generic, Show)

$(makePrisms ''AcceptError)

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
  -> m (Event t (Either AcceptError (Accept t)))
     -- ^ This event will fire exactly once.
accept (AcceptConfig mHost mService listenQueue options eClose) = do
  (eAccept, onAccept) <- newTriggerEvent
  (eClosed, onClosed) <- newTriggerEvent
  (eError, onError) <- newTriggerEvent

  isOpen <- liftIO $ STM.newEmptyTMVarIO

  let
    start = liftIO $ makeListenSocket >>= \case
      Left exc -> pure $ Left exc
      Right sock -> do
        atomically $ STM.putTMVar isOpen sock
        void $ forkIO acceptLoop
        pure . Right $ Accept eAccept eClosed eError

      where
        makeListenSocket =
          let
            getAddrs :: ExceptT AcceptError IO (NonEmpty AddrInfo)
            getAddrs = withExceptT GetAddrInfoError . ExceptT . try $
              -- fromList is OK here, as getaddrinfo(3) is required to
              -- return a nonempty list of addrinfos.
              --
              -- See: http://pubs.opengroup.org/onlinepubs/9699919799/functions/getaddrinfo.html
              -- And: https://github.com/haskell/network/issues/407
              fromList <$> NS.getAddrInfo (Just passiveHints) mHost mService
              where passiveHints = NS.defaultHints { addrFlags = [AI_PASSIVE] }

            tryListen
              :: AddrInfo
              -> ExceptT (NonEmpty (AddrInfo, IOException)) IO Socket
            tryListen info = withExceptT (pure . (info,)) . ExceptT . try $ do
              sock <- NS.socket (addrFamily info) NS.Stream NS.defaultProtocol
              (`onException` NS.close sock) $ do
                traverse_ (uncurry $ NS.setSocketOption sock) options
                NS.bind sock (addrAddress info)
                NS.listen sock listenQueue
              pure sock

          in runExceptT $ do
            addrs <- getAddrs
            let attempts = tryListen <$> addrs
            withExceptT BindError $ asum1 attempts

        acceptLoop =
          let
            -- If we receive an exception when trying to accept, check
            -- the TMVar that's meant to hold our socket. If it's
            -- empty, then 'eClose' must have fired (and the socket
            -- closed under us) and we should go quietly. Otherwise,
            -- close the socket ourselves and signal 'eError'.
            exHandlerAccept e = atomically (STM.tryReadTMVar isOpen)
              >>= maybe (pure ()) (const $ close *> onError e)
          in do
            atomically (STM.tryReadTMVar isOpen) >>= \case
              Nothing -> onClosed ()
              Just sock -> do
                try (NS.accept sock) >>= either exHandlerAccept onAccept
                acceptLoop

    close = atomically (STM.tryTakeTMVar isOpen) >>= traverse_ NS.close

  performEvent_ $ eClose $> liftIO close

  ePostBuild <- getPostBuild
  performEvent $ ePostBuild $> start
