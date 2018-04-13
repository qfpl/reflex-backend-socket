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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (
    main
  ) where

import Control.Monad (void)
import Control.Monad.Fix (MonadFix)

import Control.Monad.Trans (MonadIO)

import qualified Network.Socket as NS

import qualified Data.Map as Map
import qualified Data.ByteString as B

import Reflex

import Reflex.Host.Basic
import Reflex.Host.Subhost
import Reflex.Server.Socket

import Common

type ServerCxt t m =
  ( Reflex t
  , MonadFix m
  , MonadSample t m
  , PerformEvent t m
  , PostBuild t m
  , TriggerEvent t m
  , MonadIO m
  , MonadIO (Performable m)
  )

perConnection :: ServerCxt t m => Dynamic t NS.Socket -> Event t [B.ByteString] -> m (Event t B.ByteString, Event t ())
perConnection dv' eIn' = mdo
  s <- sample . current $ dv'

  let
    sc = SocketConfig s 4096 eIn' eQuit
  so <- socket sc
  let
    eRx = _sRecieve so
    eClosed = _sClosed so
    eQuit = leftmost [void . ffilter (== "quit") $ eRx, eClosed]
    eOut = ffilter (/= "quit") eRx

  pure (eOut, eQuit)

go :: forall t m. BasicGuest t m ((), Event t ())
go = mdo
  a <- accept $ AcceptConfig (Just "127.0.0.1") (Just "9000") 1 eQuit

  let eAccept = fst <$> _aAcceptSocket a
  eIns <- numberOccurrences eAccept

  dMap <- foldDyn ($) Map.empty . mergeWith (.) $ [
            uncurry Map.insert <$> eIns
          , flip (foldr Map.delete) <$> eRemoves
          ]

  dme <- list dMap $ \dv ->
    subhost dv eIn perConnection

  let
    eIn :: Event t [B.ByteString] = fmap Map.elems . switchDyn . fmap (mergeMap . fmap fst) $ dme
    eRemoves = fmap Map.keys . switchDyn . fmap (mergeMap . fmap snd) $ dme

    -- quit if we end up with an empty map
    eQuitMap = void . ffilter id . updated . fmap Map.null $ dMap
    eQuitListenClose = _aListenClosed a
    eQuitListenError = void $ _aError a
    eQuit = leftmost [eQuitMap, eQuitListenClose, eQuitListenError]
  pure ((), eQuit)

main :: IO ()
main =
  basicHostWithQuit go
