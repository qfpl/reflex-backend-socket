{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Reflex.Server.Socket.Scratch.ByteString where

import Control.Monad.Trans (liftIO)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Reflex

import Data.Map (Map)
import qualified Data.Map as Map

import Reflex.Server.Socket
import Reflex.Server.Socket.ByteString
import Reflex.Basic.Host

import Reflex.Server.Socket.Scratch.List

connect1 :: IO ()
connect1 = basicHost $ do
  c <- connect $ ConnectConfig (Just "127.0.0.1") (Just "9000")

  performEvent_ $ (liftIO . putStrLn $ "Connected") <$ _cSocket c
  performEvent_ $ liftIO . putStrLn <$> _cError c

  pure ()

accept1 :: IO ()
accept1 = basicHost $ do
  a <- accept $ AcceptConfig (Just "127.0.0.1") (Just "9000") 1 never

  performEvent_ $ (liftIO . putStrLn $ "Listen closed") <$ _aListenClosed a
  performEvent_ $ (liftIO . putStrLn $ "Connected") <$ _aAcceptSocket a
  performEvent_ $ liftIO . putStrLn <$> _aError a

  pure ()

connect2 :: IO ()
connect2 = basicHost $ mdo
  c <- connect $ ConnectConfig (Just "127.0.0.1") (Just "9000")

  performEvent_ $ (liftIO . putStrLn $ "Connected") <$ _cSocket c
  performEvent_ $ liftIO . putStrLn <$> _cError c

  let eConnect = _cSocket c
  dCount <- count eConnect

  dMap <- foldDyn ($) Map.empty .
          mergeWith (.) $ [
            Map.insert <$> current dCount <@> eConnect
          , flip (foldr Map.delete) <$> eRemoves
          ]

  dmeRemoves <- list dMap $ \ds -> mdo
    s <- sample . current $ ds
    let
      eTx = ["Hi"] <$ _sOpen so
      eClose = () <$ _sOpen so
    so <- socket $ SocketConfig s 2048 eTx eClose
    performEvent_ $ liftIO . BC.putStrLn <$> _sRecieve so
    performEvent_ $ liftIO . putStrLn <$> _sError so
    performEvent_ $ (liftIO . putStrLn $ "Closed") <$ _sClosed so
    pure $ () <$ _sClosed so

  let
    eRemoves = fmap Map.keys . switch . current . fmap mergeMap $ dmeRemoves

  pure ()

accept2 :: IO ()
accept2 = basicHost $ mdo
  a <- accept $ AcceptConfig (Just "127.0.0.1") (Just "9000") 1 never

  performEvent_ $ (liftIO . putStrLn $ "Listen closed") <$ _aListenClosed a
  performEvent_ $ (liftIO . putStrLn $ "Connected") <$ _aAcceptSocket a
  performEvent_ $ liftIO . putStrLn <$> _aError a

  let eAccept = fst <$> _aAcceptSocket a
  dCount <- count eAccept

  dMap <- foldDyn ($) Map.empty .
          mergeWith (.) $ [
            Map.insert <$> current dCount <@> eAccept
          , flip (foldr Map.delete) <$> eRemoves
          ]

  dmeRemoves <- list dMap $ \ds -> do
    s <- sample . current $ ds
    so <- socket $ SocketConfig s 2048 never never
    performEvent_ $ liftIO . BC.putStrLn <$> _sRecieve so
    performEvent_ $ liftIO . putStrLn <$> _sError so
    performEvent_ $ (liftIO . putStrLn $ "Closed") <$ _sClosed so
    pure $ () <$ _sClosed so

  let
    eRemoves = fmap Map.keys . switch . current . fmap mergeMap $ dmeRemoves

  pure ()

connect3 :: IO ()
connect3 = basicHost $ mdo
  c <- connect $ ConnectConfig (Just "127.0.0.1") (Just "9000")

  performEvent_ $ (liftIO . putStrLn $ "Connected") <$ _cSocket c
  performEvent_ $ liftIO . putStrLn <$> _cError c

  let eConnect = _cSocket c
  dCount <- count eConnect

  dMap <- foldDyn ($) Map.empty .
          mergeWith (.) $ [
            Map.insert <$> current dCount <@> eConnect
          , flip (foldr Map.delete) <$> eRemoves
          ]

  dmeRemoves <- list dMap $ \ds -> mdo
    s <- sample . current $ ds
    so <- socket $ SocketConfig s 2048 never (_sClosed so)
    performEvent_ $ liftIO . BC.putStrLn <$> _sRecieve so
    performEvent_ $ liftIO . putStrLn <$> _sError so
    performEvent_ $ (liftIO . putStrLn $ "Closed") <$ _sClosed so
    pure $ () <$ _sClosed so

  let
    eRemoves = fmap Map.keys . switch . current . fmap mergeMap $ dmeRemoves

  pure ()

accept3 :: IO ()
accept3 = basicHost $ mdo
  a <- accept $ AcceptConfig (Just "127.0.0.1") (Just "9000") 1 never

  performEvent_ $ (liftIO . putStrLn $ "Listen closed") <$ _aListenClosed a
  performEvent_ $ (liftIO . putStrLn $ "Connected") <$ _aAcceptSocket a
  performEvent_ $ liftIO . putStrLn <$> _aError a

  let eAccept = fst <$> _aAcceptSocket a
  dCount <- count eAccept

  dMap <- foldDyn ($) Map.empty .
          mergeWith (.) $ [
            Map.insert <$> current dCount <@> eAccept
          , flip (foldr Map.delete) <$> eRemoves
          ]

  dmeRemoves <- list dMap $ \ds -> mdo
    s <- sample . current $ ds
    let
      eTx = ["Hi"] <$ _sOpen so
      eClose = leftmost [_sOpen so, _sClosed so]
    so <- socket $ SocketConfig s 2048 eTx eClose
    performEvent_ $ liftIO . BC.putStrLn <$> _sRecieve so
    performEvent_ $ liftIO . putStrLn <$> _sError so
    performEvent_ $ (liftIO . putStrLn $ "Closed") <$ _sClosed so
    pure $ () <$ _sClosed so

  let
    eRemoves = fmap Map.keys . switch . current . fmap mergeMap $ dmeRemoves

  pure ()

connect4 :: IO ()
connect4 = basicHost $ mdo
  c <- connect $ ConnectConfig (Just "127.0.0.1") (Just "9000")

  performEvent_ $ (liftIO . putStrLn $ "Connected") <$ _cSocket c
  performEvent_ $ liftIO . putStrLn <$> _cError c

  let eConnect = _cSocket c
  dCount <- count eConnect

  dMap <- foldDyn ($) Map.empty .
          mergeWith (.) $ [
            Map.insert <$> current dCount <@> eConnect
          , flip (foldr Map.delete) <$> eRemoves
          ]

  dmeRemoves <- list dMap $ \ds -> mdo
    s <- sample . current $ ds
    let
      eTx = ["Hi"] <$ _sOpen so
      eClose = leftmost [() <$ _sRecieve so, _sClosed so]
    so <- socket $ SocketConfig s 2048 eTx eClose
    performEvent_ $ liftIO . BC.putStrLn <$> _sRecieve so
    performEvent_ $ liftIO . putStrLn <$> _sError so
    performEvent_ $ (liftIO . putStrLn $ "Closed") <$ _sClosed so
    pure $ () <$ _sClosed so

  let
    eRemoves = fmap Map.keys . switch . current . fmap mergeMap $ dmeRemoves

  pure ()

accept4 :: IO ()
accept4 = basicHost $ mdo
  a <- accept $ AcceptConfig (Just "127.0.0.1") (Just "9000") 1 never

  performEvent_ $ (liftIO . putStrLn $ "Listen closed") <$ _aListenClosed a
  performEvent_ $ (liftIO . putStrLn $ "Connected") <$ _aAcceptSocket a
  performEvent_ $ liftIO . putStrLn <$> _aError a

  let eAccept = fst <$> _aAcceptSocket a
  dCount <- count eAccept

  dMap <- foldDyn ($) Map.empty .
          mergeWith (.) $ [
            Map.insert <$> current dCount <@> eAccept
          , flip (foldr Map.delete) <$> eRemoves
          ]

  dmeRemoves <- list dMap $ \ds -> mdo
    s <- sample . current $ ds
    let
      eTx = ["Hello"] <$ _sRecieve so
    so <- socket $ SocketConfig s 2048 eTx (_sClosed so)
    performEvent_ $ liftIO . BC.putStrLn <$> _sRecieve so
    performEvent_ $ liftIO . putStrLn <$> _sError so
    performEvent_ $ (liftIO . putStrLn $ "Closed") <$ _sClosed so
    pure $ () <$ _sClosed so

  let
    eRemoves = fmap Map.keys . switch . current . fmap mergeMap $ dmeRemoves

  pure ()
