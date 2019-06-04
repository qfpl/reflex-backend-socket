{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Main (main) where

import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BC
import           Data.Functor ((<&>), void)
import           Data.Maybe (isNothing)
import           Reflex
import           Reflex.Backend.Socket
import           Reflex.Host.Basic (basicHostForever, basicHostWithQuit)
import           Reflex.Network (networkHold)
import           System.Environment (getArgs)

-- | Connect to a remote host, and quit as soon as something happens.
connect1 :: IO ()
connect1 = basicHostWithQuit $ do
  eQuit <- connect (Just "127.0.0.1") "9000"
  let (eError, eConnect) = fanEither eQuit

  performEvent_ $ (liftIO . putStrLn $ "Connected") <$ eConnect
  performEvent_ $ liftIO . print <$> eError

  pure ((), void eQuit)

-- | Listen for connections, and log them as they come in. Does
-- nothing with arriving connections, so will leak FDs.
accept1 :: IO ()
accept1 = basicHostForever $ do
  a <- accept $ AcceptConfig (Just "127.0.0.1") (Just "9000") 1 never

  performEvent_ $ (liftIO . putStrLn $ "Listen closed") <$ _aClose a
  performEvent_ $ (liftIO . putStrLn $ "Connected") <$ _aAcceptSocket a
  performEvent_ $ liftIO . print <$> _aError a

  pure ()

-- | Connect to a remote host. When the connection succeeds, put the
-- @'Socket' t@ into @dSocket@, send a message, then close.
connect2 :: IO ()
connect2 = basicHostWithQuit $ mdo
  (eConnError, eConnect) <- fanEither <$> connect (Just "127.0.0.1") "9000"

  performEvent_ $ (liftIO . putStrLn $ "Connected") <$ eConnect
  performEvent_ $ liftIO . print <$> eConnError

  dSocket :: Dynamic t (Maybe (Socket t)) <- networkHold (pure Nothing) $ leftmost
    [ eConnect <&> \s -> Just <$> socket (SocketConfig s 2048 eTx eOpen)
    , pure Nothing <$ eClosed
    ]

  eOpen <- switchHold never . fmap _sOpen . mapMaybe id $ updated dSocket
  let eTx = "Hi" <$ eOpen

  eClosed <- switchHold never . fmap _sClose . mapMaybe id $ updated dSocket
  eRx <- switchHold never . fmap _sReceive . mapMaybe id $ updated dSocket
  eSockError <- switchHold never . fmap _sError . mapMaybe id $ updated dSocket

  performEvent_ $ liftIO . BC.putStrLn <$> eRx
  performEvent_ $ liftIO . print <$> eSockError
  performEvent_ $ liftIO (putStrLn "Closed") <$ eClosed

  let eQuit = leftmost
        [ void eConnError
        , void . ffilter isNothing $ updated dSocket
        ]

  pure ((), eQuit)

main :: IO ()
main = getArgs >>= \case
  ["accept1"] -> accept1
  ["connect1"] -> connect1
  ["connect2"] -> connect2
  _ -> pure ()
