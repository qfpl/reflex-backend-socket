{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Main (
    main
  ) where

import Control.Monad (void, forever)
import Control.Monad.Fix (MonadFix)
import Control.Concurrent (forkIO)

import Control.Monad.Trans (MonadIO(..))

import qualified Network.Socket as NS

import qualified Data.ByteString.Char8 as BC

import Reflex
import Reflex.Workflow

import Reflex.Host.Basic
import Reflex.Server.Socket

import Common

main :: IO ()
main = basicHostWithQuit $ do
  deQuit <- workflow unconnected
  pure ((), switchDyn deQuit)

type ClientCxt t m =
  ( Reflex t
  , MonadFix m
  , PerformEvent t m
  , PostBuild t m
  , TriggerEvent t m
  , MonadIO m
  , MonadIO (Performable m)
  )

unconnected :: ClientCxt t m => Workflow t m (Event t ())
unconnected = Workflow $ do
  c <- connect $ ConnectConfig (Just "127.0.0.1") (Just "9000")
  -- TODO need to check for errors and quit appropriately here
  pure (never, connected <$> _cSocket c)

connected :: ClientCxt t m => NS.Socket -> Workflow t m (Event t ())
connected s = Workflow $ mdo
  (eLine, onLine) <- newTriggerEvent
  void . liftIO . forkIO . forever $
    onLine . pure . BC.pack =<< getLine

  let
    sc = SocketConfig s 2048 eLine eQuit
  so <- socket sc
  performEvent_ $ liftIO . putStrLn . BC.unpack <$> _sRecieve so
  let
    eQuit = _sClosed so

  pure (eQuit, unconnected <$ eQuit)

