{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}
module Examples.ByteString where

import Control.Monad (void)

import Control.Monad.Trans (liftIO)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Reflex

import qualified Data.Map as Map

import Reflex.Host.Basic
import Reflex.Backend.Socket

connect1 :: IO ()
connect1 = basicHostWithQuit $ do
  c <- connect $ ConnectConfig (Just "127.0.0.1") (Just "9000")

  performEvent_ $ (liftIO . putStrLn $ "Connected") <$ _cSocket c
  performEvent_ $ liftIO . putStrLn <$> _cError c

  let
    eQuit = leftmost [void (_cSocket c), void (_cError c)]

  pure (() , eQuit)

accept1 :: IO ()
accept1 = basicHostForever $ do
  a <- accept $ AcceptConfig (Just "127.0.0.1") (Just "9000") 1 never

  performEvent_ $ (liftIO . putStrLn $ "Listen closed") <$ _aListenClosed a
  performEvent_ $ (liftIO . putStrLn $ "Connected") <$ _aAcceptSocket a
  performEvent_ $ liftIO . putStrLn <$> _aError a

  pure ()

connect2 :: IO ()
connect2 = basicHostWithQuit connect2'

connect2' :: forall t m. BasicGuest t m ((), Event t ())
connect2' = mdo
  c <- connect $ ConnectConfig (Just "127.0.0.1") (Just "9000")

  performEvent_ $ (liftIO . putStrLn $ "Connected") <$ _cSocket c
  performEvent_ $ liftIO . putStrLn <$> _cError c

  let eConnect = _cSocket c
  dCount :: Dynamic t Int <- count eConnect

  dMap <- foldDyn ($) Map.empty .
          mergeWith (.) $ [
            Map.insert <$> current dCount <@> eConnect
          , flip (foldr Map.delete) <$> eRemoves
          ]

  dmeRemoves <- list dMap $ \ds -> mdo
    s <- sample . current $ ds
    let
      eTx = ["Hi" :: B.ByteString] <$ _sOpen so
      eClose = () <$ _sOpen so
    so <- socket $ SocketConfig s 2048 eTx eClose

    performEvent_ $ liftIO . BC.putStrLn <$> _sRecieve so
    performEvent_ $ liftIO . putStrLn <$> _sError so
    performEvent_ $ (liftIO . putStrLn $ "Closed") <$ _sClosed so
    pure $ () <$ _sClosed so

  let
    eRemoves = fmap Map.keys . switch . current . fmap mergeMap $ dmeRemoves
    eQuit = void . ffilter (not . null) $ eRemoves

  pure (() , eQuit)

accept2 :: IO ()
accept2 = basicHostForever accept2'

accept2' :: forall t m. BasicGuest t m ()
accept2' = mdo
  a <- accept $ AcceptConfig (Just "127.0.0.1") (Just "9000") 1 never

  performEvent_ $ (liftIO . putStrLn $ "Listen closed") <$ _aListenClosed a
  performEvent_ $ (liftIO . putStrLn $ "Connected") <$ _aAcceptSocket a
  performEvent_ $ liftIO . putStrLn <$> _aError a

  let eAccept = fst <$> _aAcceptSocket a
  dCount :: Dynamic t Int <- count eAccept

  dMap <- foldDyn ($) Map.empty .
          mergeWith (.) $ [
            Map.insert <$> current dCount <@> eAccept
          , flip (foldr Map.delete) <$> eRemoves
          ]

  dmeRemoves <- list dMap $ \ds -> do
    s <- sample . current $ ds

    so <- socket $ SocketConfig s 2048 (([] :: [B.ByteString]) <$ never) never

    performEvent_ $ liftIO . BC.putStrLn <$> _sRecieve so
    performEvent_ $ liftIO . putStrLn <$> _sError so
    performEvent_ $ (liftIO . putStrLn $ "Closed") <$ _sClosed so

    pure $ () <$ _sClosed so

  let
    eRemoves = fmap Map.keys . switch . current . fmap mergeMap $ dmeRemoves

  pure ()

connect3 :: IO ()
connect3 = basicHostWithQuit connect3'

connect3' :: forall t m. BasicGuest t m ((), Event t ())
connect3' = mdo
  c <- connect $ ConnectConfig (Just "127.0.0.1") (Just "9000")

  performEvent_ $ (liftIO . putStrLn $ "Connected") <$ _cSocket c
  performEvent_ $ liftIO . putStrLn <$> _cError c

  let eConnect = _cSocket c
  dCount :: Dynamic t Int <- count eConnect

  dMap <- foldDyn ($) Map.empty .
          mergeWith (.) $ [
            Map.insert <$> current dCount <@> eConnect
          , flip (foldr Map.delete) <$> eRemoves
          ]

  dmeRemoves <- list dMap $ \ds -> mdo
    s <- sample . current $ ds
    so <- socket $ SocketConfig s 2048 (([] :: [B.ByteString]) <$ never) (_sClosed so)
    performEvent_ $ liftIO . BC.putStrLn <$> _sRecieve so
    performEvent_ $ liftIO . putStrLn <$> _sError so
    performEvent_ $ (liftIO . putStrLn $ "Closed") <$ _sClosed so
    pure $ () <$ _sClosed so

  let
    eRemoves = fmap Map.keys . switch . current . fmap mergeMap $ dmeRemoves
    eQuit = void . ffilter (not . null) $ eRemoves

  pure (() , eQuit)

accept3 :: IO ()
accept3 = basicHostForever accept3'

accept3' :: forall t m. BasicGuest t m ()
accept3' = mdo
  a <- accept $ AcceptConfig (Just "127.0.0.1") (Just "9000") 1 never

  performEvent_ $ (liftIO . putStrLn $ "Listen closed") <$ _aListenClosed a
  performEvent_ $ (liftIO . putStrLn $ "Connected") <$ _aAcceptSocket a
  performEvent_ $ liftIO . putStrLn <$> _aError a

  let eAccept = fst <$> _aAcceptSocket a
  dCount :: Dynamic t Int <- count eAccept

  dMap <- foldDyn ($) Map.empty .
          mergeWith (.) $ [
            Map.insert <$> current dCount <@> eAccept
          , flip (foldr Map.delete) <$> eRemoves
          ]

  dmeRemoves <- list dMap $ \ds -> mdo
    s <- sample . current $ ds
    let
      eTx = ["Hi" :: B.ByteString] <$ _sOpen so
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
connect4 = basicHostWithQuit connect4'

connect4' :: forall t m. BasicGuest t m ((), Event t ())
connect4' = mdo
  c <- connect $ ConnectConfig (Just "127.0.0.1") (Just "9000")

  performEvent_ $ (liftIO . putStrLn $ "Connected") <$ _cSocket c
  performEvent_ $ liftIO . putStrLn <$> _cError c

  let eConnect = _cSocket c
  dCount :: Dynamic t Int <- count eConnect

  dMap <- foldDyn ($) Map.empty .
          mergeWith (.) $ [
            Map.insert <$> current dCount <@> eConnect
          , flip (foldr Map.delete) <$> eRemoves
          ]

  dmeRemoves <- list dMap $ \ds -> mdo
    s <- sample . current $ ds
    let
      eTx = ["Hi" :: B.ByteString] <$ _sOpen so
      eClose = leftmost [() <$ _sRecieve so, _sClosed so]
    so <- socket $ SocketConfig s 2048 eTx eClose
    performEvent_ $ liftIO . BC.putStrLn <$> _sRecieve so
    performEvent_ $ liftIO . putStrLn <$> _sError so
    performEvent_ $ (liftIO . putStrLn $ "Closed") <$ _sClosed so
    pure $ () <$ _sClosed so

  let
    eRemoves = fmap Map.keys . switch . current . fmap mergeMap $ dmeRemoves
    eQuit = void . ffilter (not . null) $ eRemoves

  pure (() , eQuit)

accept4 :: IO ()
accept4 = basicHostForever accept4'

accept4' :: forall t m. BasicGuest t m ()
accept4' = mdo
  a <- accept $ AcceptConfig (Just "127.0.0.1") (Just "9000") 1 never

  performEvent_ $ (liftIO . putStrLn $ "Listen closed") <$ _aListenClosed a
  performEvent_ $ (liftIO . putStrLn $ "Connected") <$ _aAcceptSocket a
  performEvent_ $ liftIO . putStrLn <$> _aError a

  let eAccept = fst <$> _aAcceptSocket a
  dCount :: Dynamic t Int <- count eAccept

  dMap <- foldDyn ($) Map.empty .
          mergeWith (.) $ [
            Map.insert <$> current dCount <@> eAccept
          , flip (foldr Map.delete) <$> eRemoves
          ]

  dmeRemoves <- list dMap $ \ds -> mdo
    s <- sample . current $ ds
    let
      eTx = ["Hello" :: B.ByteString] <$ _sRecieve so
    so <- socket $ SocketConfig s 2048 eTx (_sClosed so)
    performEvent_ $ liftIO . BC.putStrLn <$> _sRecieve so
    performEvent_ $ liftIO . putStrLn <$> _sError so
    performEvent_ $ (liftIO . putStrLn $ "Closed") <$ _sClosed so
    pure $ () <$ _sClosed so

  let
    eRemoves = fmap Map.keys . switch . current . fmap mergeMap $ dmeRemoves

  pure ()
