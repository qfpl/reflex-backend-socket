{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Examples.Binary where

import Control.Monad (void)
import Control.Monad.Trans (liftIO)

import Data.Binary

import qualified Data.Map as Map

import Reflex

import Reflex.Host.Basic
import Reflex.Binary
import Reflex.Backend.Socket

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
connect1 = basicHostWithQuit $ do
  c <- connect $ ConnectConfig (Just "127.0.0.1") (Just "9000")

  performEvent_ $ (liftIO . putStrLn $ "Connected") <$ _cSocket c
  performEvent_ $ liftIO . putStrLn <$> _cError c

  let 
    eQuit = leftmost [void (_cSocket c), void (_cError c)]

  pure ((), eQuit)

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
      eTx = [Client1 "Hi"] <$ _sOpen so
      eClose = () <$ _sOpen so
      sc = SocketConfig s 2048 eTx eClose
    so :: Socket t ServerMessage <- socket (sc :: SocketConfig t ClientMessage)
    performEvent_ $ liftIO . print <$> _sRecieve so
    performEvent_ $ liftIO . putStrLn <$> _sError so
    performEvent_ $ (liftIO . putStrLn $ "Closed") <$ _sClosed so
    pure $ () <$ _sClosed so

  let
    eRemoves = fmap Map.keys . switch . current . fmap mergeMap $ dmeRemoves
    eQuit = void . ffilter (not . null) $ eRemoves

  pure ((), eQuit)

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
    let
      sc = SocketConfig s 2048 never never
    so :: Socket t ClientMessage <- socket (sc :: SocketConfig t ServerMessage)
    performEvent_ $ liftIO . print <$> _sRecieve so
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
    let
      sc = SocketConfig s 2048 never (void $ _sRecieve so)
    so :: Socket t ServerMessage <- socket (sc :: SocketConfig t ClientMessage)
    performEvent_ $ liftIO . print <$> _sRecieve so
    performEvent_ $ liftIO . putStrLn <$> _sError so
    performEvent_ $ (liftIO . putStrLn $ "Closed") <$ _sClosed so
    pure $ () <$ _sClosed so

  let
    eRemoves = fmap Map.keys . switch . current . fmap mergeMap $ dmeRemoves
    eQuit = void . ffilter (not . null) $ eRemoves

  pure ((), eQuit)

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
      eTx = [Server1 "Hi"] <$ _sOpen so
      -- eClose = leftmost [_sOpen so, _sClosed so]
      eClose = _sClosed so
      sc = SocketConfig s 2048 eTx eClose
    so :: Socket t ClientMessage <- socket sc
    performEvent_ $ liftIO . print <$> _sRecieve so
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
      eTx = [Client1 "Hi", Client1 "Hi again"] <$ _sOpen so
      eClose = leftmost [void . ffilter id . updated $ dRxDone, _sClosed so]
      sc = SocketConfig s 2048 eTx eClose
    so :: Socket t ServerMessage <- socket sc
    dRxCount :: Dynamic t Int <- count $ _sRecieve so
    let dRxDone = (>= 2) <$> dRxCount
    performEvent_ $ liftIO . print <$> _sRecieve so
    performEvent_ $ liftIO . putStrLn <$> _sError so
    performEvent_ $ (liftIO . putStrLn $ "Closed") <$ _sClosed so
    pure $ () <$ _sClosed so

  let
    eRemoves = fmap Map.keys . switch . current . fmap mergeMap $ dmeRemoves
    eQuit = void . ffilter (not . null) $ eRemoves

  pure ((), eQuit)

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
      eTx = [Server1 "Hello"] <$ _sRecieve so
      sc = SocketConfig s 2048 eTx (_sClosed so)
    so :: Socket t ClientMessage <- socket sc
    performEvent_ $ liftIO . print <$> _sRecieve so
    performEvent_ $ liftIO . putStrLn <$> _sError so
    performEvent_ $ (liftIO . putStrLn $ "Closed") <$ _sClosed so
    pure $ () <$ _sClosed so

  let
    eRemoves = fmap Map.keys . switch . current . fmap mergeMap $ dmeRemoves

  pure ()
