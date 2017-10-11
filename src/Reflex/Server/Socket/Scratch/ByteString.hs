{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Reflex.Server.Socket.Scratch.ByteString where

import Control.Monad.Trans (liftIO)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Reflex

import Reflex.Server.Socket
import Reflex.Server.Socket.ByteString
import Reflex.Basic.Host

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

connect2 :: IO ()
connect2 = basicHost $ do
  c <- connect $ ConnectConfig (Just "127.0.0.1") (Just "9000")

  performEvent_ $ (liftIO . putStrLn $ "Connected") <$ _cSocket c
  performEvent_ $ liftIO . putStrLn <$> _cError c

  let
    eTx = ["Hi"] <$ _cSocket c
    eClose = () <$ _cSocket c
  s <- socket $ SocketConfig 2048 (_cSocket c) eTx eClose
  performEvent_ $ liftIO . BC.putStrLn <$> _soRecieve s
  performEvent_ $ liftIO . putStrLn <$> _soError s
  performEvent_ $ (liftIO . putStrLn $ "Closed") <$ _soClosed s

  pure ()

accept2 :: IO ()
accept2 = basicHost $ do
  a <- accept $ AcceptConfig (Just "127.0.0.1") (Just "9000") 1 never

  performEvent_ $ (liftIO . putStrLn $ "Listen connected") <$ _aListenSocket a
  performEvent_ $ (liftIO . putStrLn $ "Listen closed") <$ _aListenClosed a
  performEvent_ $ (liftIO . putStrLn $ "Connected") <$ _aAcceptSocket a
  performEvent_ $ liftIO . putStrLn <$> _aError a

  s <- socket $ SocketConfig 2048 (fst <$> _aAcceptSocket a) never never
  performEvent_ $ liftIO . BC.putStrLn <$> _soRecieve s
  performEvent_ $ liftIO . putStrLn <$> _soError s
  performEvent_ $ (liftIO . putStrLn $ "Closed") <$ _soClosed s

  pure ()

connect3 :: IO ()
connect3 = basicHost $ mdo
  c <- connect $ ConnectConfig (Just "127.0.0.1") (Just "9000")

  performEvent_ $ (liftIO . putStrLn $ "Connected") <$ _cSocket c
  performEvent_ $ liftIO . putStrLn <$> _cError c

  s <- socket $ SocketConfig 2048 (_cSocket c) never (_soClosed s)
  performEvent_ $ liftIO . BC.putStrLn <$> _soRecieve s
  performEvent_ $ liftIO . putStrLn <$> _soError s
  performEvent_ $ (liftIO . putStrLn $ "Closed") <$ _soClosed s

  pure ()

accept3 :: IO ()
accept3 = basicHost $ mdo
  a <- accept $ AcceptConfig (Just "127.0.0.1") (Just "9000") 1 never

  performEvent_ $ (liftIO . putStrLn $ "Listen connected") <$ _aListenSocket a
  performEvent_ $ (liftIO . putStrLn $ "Listen closed") <$ _aListenClosed a
  performEvent_ $ (liftIO . putStrLn $ "Connected") <$ _aAcceptSocket a
  performEvent_ $ liftIO . putStrLn <$> _aError a

  let
    eTx = ["Hi"] <$ _aAcceptSocket a
    eClose = leftmost [() <$ _aAcceptSocket a, _soClosed s]
  s <- socket $ SocketConfig 2048 (fst <$> _aAcceptSocket a) eTx eClose
  performEvent_ $ liftIO . BC.putStrLn <$> _soRecieve s
  performEvent_ $ liftIO . putStrLn <$> _soError s
  performEvent_ $ (liftIO . putStrLn $ "Closed") <$ _soClosed s

  pure ()

connect4 :: IO ()
connect4 = basicHost $ mdo
  c <- connect $ ConnectConfig (Just "127.0.0.1") (Just "9000")

  performEvent_ $ (liftIO . putStrLn $ "Connected") <$ _cSocket c
  performEvent_ $ liftIO . putStrLn <$> _cError c

  let
    eTx = ["Hi"] <$ _cSocket c
    eClose = leftmost [() <$ _soRecieve s, _soClosed s]
  s <- socket $ SocketConfig 2048 (_cSocket c) eTx eClose
  performEvent_ $ liftIO . BC.putStrLn <$> _soRecieve s
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
    eTx = ["Hello"] <$ _soRecieve s
  s <- socket $ SocketConfig 2048 (fst <$> _aAcceptSocket a) eTx (_soClosed s)
  performEvent_ $ liftIO . BC.putStrLn <$> _soRecieve s
  performEvent_ $ liftIO . putStrLn <$> _soError s
  performEvent_ $ (liftIO . putStrLn $ "Closed") <$ _soClosed s

  pure ()
