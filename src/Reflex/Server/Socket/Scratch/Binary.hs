{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Reflex.Server.Socket.Scratch.Binary where

import Control.Monad.Trans (liftIO)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Data.Binary

import Reflex

import Reflex.Server.Socket
import Reflex.Server.Socket.Binary
import Reflex.Basic.Host

data ClientMessage =
  Client1 String
  deriving (Eq, Ord, Show)

instance Binary ClientMessage where
  put im =
    case im of
      Client1 s -> put (0 :: Word8) >> put s
  get = do
    c <- get :: Get Word8
    case c of
      0 -> Client1 <$> get
      _ -> error "unknown ClientMessage"

data ServerMessage =
  Server1 String
  deriving (Eq, Ord, Show)

instance Binary ServerMessage where
  put om =
    case om of
      Server1 s -> put (0 :: Word8) >> put s
  get = do
    c <- get :: Get Word8
    case c of
      0 -> Server1 <$> get
      _ -> error "unknown ServerMessage"

connect1 :: IO ()
connect1 = basicHost $ do
  c <- connect $ ConnectConfig (Just "127.0.0.1") (Just "9000")

  performEvent_ $ (liftIO . putStrLn $ "Connected") <$ _cSocket c
  performEvent_ $ liftIO . putStrLn <$> _cError c

  pure ()

accept1 :: IO ()
accept1 = basicHost $ do
  a <- accept $ AcceptConfig (Just "127.0.0.1") (Just "9000") 1 never

  performEvent_ $ (liftIO . putStrLn $ "Listen connected") <$ _aListenSocket a
  performEvent_ $ (liftIO . putStrLn $ "Listen closed") <$ _aListenClosed a
  performEvent_ $ (liftIO . putStrLn $ "Connected") <$ _aAcceptSocket a
  performEvent_ $ liftIO . putStrLn <$> _aError a

  pure ()

connect2' :: forall t m. BasicGuest t m ()
connect2' = do
  c <- connect $ ConnectConfig (Just "127.0.0.1") (Just "9000")

  performEvent_ $ (liftIO . putStrLn $ "Connected") <$ _cSocket c
  performEvent_ $ liftIO . putStrLn <$> _cError c

  let
    eTx = [Client1 "Hi"] <$ _cSocket c
    eClose = () <$ _cSocket c
    sc = SocketConfig 2048 (_cSocket c) eTx eClose
  s :: SocketOut t ServerMessage <- socket (sc :: SocketConfig t ClientMessage)
  performEvent_ $ liftIO . print <$> _soRecieve s
  performEvent_ $ liftIO . putStrLn <$> _soError s
  performEvent_ $ (liftIO . putStrLn $ "Closed") <$ _soClosed s

  pure ()

connect2 :: IO ()
connect2 = basicHost connect2'

accept2' :: forall t m. BasicGuest t m ()
accept2' = do
  a <- accept $ AcceptConfig (Just "127.0.0.1") (Just "9000") 1 never

  performEvent_ $ (liftIO . putStrLn $ "Listen connected") <$ _aListenSocket a
  performEvent_ $ (liftIO . putStrLn $ "Listen closed") <$ _aListenClosed a
  performEvent_ $ (liftIO . putStrLn $ "Connected") <$ _aAcceptSocket a
  performEvent_ $ liftIO . putStrLn <$> _aError a

  let
    sc = SocketConfig 2048 (fst <$> _aAcceptSocket a) never never
  s :: SocketOut t ClientMessage <- socket (sc :: SocketConfig t ServerMessage)
  performEvent_ $ liftIO . print <$> _soRecieve s
  performEvent_ $ liftIO . putStrLn <$> _soError s
  performEvent_ $ (liftIO . putStrLn $ "Closed") <$ _soClosed s

  pure ()

accept2 :: IO ()
accept2 = basicHost accept2'

connect3' :: forall t m. BasicGuest t m ()
connect3' = mdo
  c <- connect $ ConnectConfig (Just "127.0.0.1") (Just "9000")

  performEvent_ $ (liftIO . putStrLn $ "Connected") <$ _cSocket c
  performEvent_ $ liftIO . putStrLn <$> _cError c

  let
    sc = SocketConfig 2048 (_cSocket c) never (_soClosed s)
  s :: SocketOut t ServerMessage <- socket (sc :: SocketConfig t ClientMessage)
  performEvent_ $ liftIO . print <$> _soRecieve s
  performEvent_ $ liftIO . putStrLn <$> _soError s
  performEvent_ $ (liftIO . putStrLn $ "Closed") <$ _soClosed s

  pure ()

connect3 :: IO ()
connect3 = basicHost connect3'

accept3' :: forall t m. BasicGuest t m ()
accept3' = mdo
  a <- accept $ AcceptConfig (Just "127.0.0.1") (Just "9000") 1 never

  performEvent_ $ (liftIO . putStrLn $ "Listen connected") <$ _aListenSocket a
  performEvent_ $ (liftIO . putStrLn $ "Listen closed") <$ _aListenClosed a
  performEvent_ $ (liftIO . putStrLn $ "Connected") <$ _aAcceptSocket a
  performEvent_ $ liftIO . putStrLn <$> _aError a

  let
    eTx = [Server1 "Hi"] <$ _aAcceptSocket a
    eClose = leftmost [() <$ _aAcceptSocket a, _soClosed s]
    sc = SocketConfig 2048 (fst <$> _aAcceptSocket a) eTx eClose
  s :: SocketOut t ClientMessage <- socket sc
  performEvent_ $ liftIO . print <$> _soRecieve s
  performEvent_ $ liftIO . putStrLn <$> _soError s
  performEvent_ $ (liftIO . putStrLn $ "Closed") <$ _soClosed s

  pure ()

accept3 :: IO ()
accept3 = basicHost accept3'

connect4 :: IO ()
connect4 = basicHost $ mdo
  c <- connect $ ConnectConfig (Just "127.0.0.1") (Just "9000")

  performEvent_ $ (liftIO . putStrLn $ "Connected") <$ _cSocket c
  performEvent_ $ liftIO . putStrLn <$> _cError c

  let
    eTx = [Client1 "Hi"] <$ _cSocket c
    eClose = leftmost [() <$ _soRecieve s, _soClosed s]
    sc = SocketConfig 2048 (_cSocket c) eTx eClose
  s :: SocketOut t ServerMessage <- socket sc
  performEvent_ $ liftIO . print <$> _soRecieve s
  performEvent_ $ liftIO . putStrLn <$> _soError s
  performEvent_ $ (liftIO . putStrLn $ "Closed") <$ _soClosed s

  pure ()

accept4 :: IO ()
accept4 = basicHost $ mdo
  a <- accept $ AcceptConfig (Just "127.0.0.1") (Just "9000") 1 never

  performEvent_ $ (liftIO . putStrLn $ "Listen connected") <$ _aListenSocket a
  performEvent_ $ (liftIO . putStrLn $ "Listen closed") <$ _aListenClosed a
  performEvent_ $ (liftIO . putStrLn $ "Connected") <$ _aAcceptSocket a
  performEvent_ $ liftIO . putStrLn <$> _aError a

  let
    eTx = [Server1 "Hello"] <$ _soRecieve s
    sc = SocketConfig 2048 (fst <$> _aAcceptSocket a) eTx (_soClosed s)
  s :: SocketOut t ClientMessage <- socket sc
  performEvent_ $ liftIO . print <$> _soRecieve s
  performEvent_ $ liftIO . putStrLn <$> _soError s
  performEvent_ $ (liftIO . putStrLn $ "Closed") <$ _soClosed s

  pure ()
