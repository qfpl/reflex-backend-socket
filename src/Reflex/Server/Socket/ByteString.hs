{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE FlexibleContexts #-}
module Reflex.Server.Socket.ByteString (
    SocketConfig(..)
  , Socket(..)
  , socket
  ) where

import Control.Monad.Trans (MonadIO)

import qualified Data.ByteString as B

import Reflex

import Reflex.Server.Socket.Common
import Reflex.Server.Socket.Internal

bytestringTx :: MkTx B.ByteString
bytestringTx = MkTx id

bytestringRx :: MkRx B.ByteString
bytestringRx =
  let
    stepRx onError onRx bs _ = do
      onRx bs
      pure (Just ())
  in
    MkRx () stepRx

socket ::
  ( Reflex t
  , PerformEvent t m
  , PostBuild t m
  , TriggerEvent t m
  , MonadIO (Performable m)
  , MonadIO m
  ) =>
  SocketConfig t B.ByteString ->
  m (Socket t B.ByteString)
socket =
  mkSocket bytestringTx bytestringRx
