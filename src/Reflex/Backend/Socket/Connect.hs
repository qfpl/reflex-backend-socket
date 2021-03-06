{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}

{-|
Module      : Reflex.Backend.Socket.Connect
Copyright   : (c) 2018-2019, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com, jack.kelly@data61.csiro.au
Stability   : experimental
Portability : non-portable

Use 'connect' to attempt a connection to a remote endpoint, and get an
@'Event' t ('Either' 'SetupError' 'Socket')@ that tells you whether or
not it worked.
-}

module Reflex.Backend.Socket.Connect (connect) where

import           Control.Concurrent (forkIO)
import           Control.Exception (IOException, onException, try)
import           Control.Monad.Except (ExceptT(..), runExceptT, withExceptT)
import           Control.Monad.Trans (MonadIO(..))
import           Data.Functor (($>), void)
import           Data.List.NonEmpty (NonEmpty, fromList)
import           Data.Semigroup.Foldable (asum1)
import           Network.Socket (Socket, AddrInfo(..), defaultProtocol)
import qualified Network.Socket as NS
import           Reflex
import           Reflex.Backend.Socket.Error (SetupError(..))

-- | Connect to a remote endpoint. The connection happens in a
-- background thread.
connect
  :: ( Reflex t
     , PerformEvent t m
     , TriggerEvent t m
     , PostBuild t m
     , MonadIO (Performable m)
     , MonadIO m
     )
  => Maybe NS.HostName
     -- ^ Host to connect to. If 'Nothing', connect via loopback.
  -> NS.ServiceName
     -- ^ Service (port number or service name). See the
     -- <https://linux.die.net/man/3/getaddrinfo manpage for getaddrinfo>.
  -> m (Event t (Either SetupError Socket))
     -- ^ This event will fire exactly once.
connect mHost service = do
  ePostBuild <- getPostBuild
  performEventAsync $ ePostBuild $> \onRes -> void . liftIO . forkIO $
    let
      getAddrs :: ExceptT SetupError IO (NonEmpty AddrInfo)
      getAddrs = withExceptT GetAddrInfoError . ExceptT . try $
        -- fromList is OK here, as getaddrinfo(3) is required to
        -- return a nonempty list of addrinfos.
        --
        -- See: http://pubs.opengroup.org/onlinepubs/9699919799/functions/getaddrinfo.html
        -- And: https://github.com/haskell/network/issues/407
        fromList <$> NS.getAddrInfo Nothing mHost (Just service)

      tryConnect
        :: AddrInfo
        -> ExceptT (NonEmpty (AddrInfo, IOException)) IO Socket
      tryConnect info = withExceptT (pure . (info,)) . ExceptT . try $ do
        sock <- NS.socket (addrFamily info) NS.Stream defaultProtocol
        NS.connect sock (addrAddress info) `onException` NS.close sock
        pure sock

    in do
      res <- runExceptT $ do
        addrs <- getAddrs
        let attempts = tryConnect <$> addrs
        withExceptT UseAddrInfoError $ asum1 attempts
      onRes res
