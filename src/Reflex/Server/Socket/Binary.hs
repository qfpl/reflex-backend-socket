{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Reflex.Server.Socket.Binary (
    SocketConfig(..)
  , Socket(..)
  , socket
  ) where

import Control.Monad.Trans (MonadIO)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB

import Data.Binary
import Data.Binary.Get

import Reflex

import Reflex.Server.Socket.Common
import Reflex.Server.Socket.Internal

binaryTx :: Binary a => MkTx a
binaryTx = MkTx (LB.toStrict . encode)

binaryRx :: forall b. Binary b => MkRx b
binaryRx =
  let
    initRx = runGetIncremental (get :: Get b)

    handleDecoding onError _ (Fail _ _ s) = do
      onError s
      pure Nothing
    handleDecoding _ _ (Partial f) =
      pure . Just $ Partial f
    handleDecoding onError onRx (Done bs _ a) = do
      onRx a
      let decoder = initRx
      if B.null bs
      then pure . Just $ decoder
      else stepRx onError onRx bs decoder

    stepRx onError onRx bs decoder =
      handleDecoding onError onRx $ pushChunk decoder bs
  in
    MkRx initRx stepRx

socket ::
  forall t m a b.
  ( Reflex t
  , PerformEvent t m
  , PostBuild t m
  , TriggerEvent t m
  , MonadIO (Performable m)
  , MonadIO m
  , Binary a
  , Binary b
  ) =>
  SocketConfig t a ->
  m (Socket t b)
socket =
  mkSocket binaryTx binaryRx
