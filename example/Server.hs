{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Copyright   : (c) 2018-2019, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}

module Main (main) where

import           Control.Monad (void)
import           Control.Monad.Fix (MonadFix)
import           Control.Monad.IO.Class (MonadIO)
import           Data.ByteString (ByteString)
import           Data.Function ((&))
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Network.Socket as NS
import           Reflex
import           Reflex.Backend.Socket
import           Reflex.Host.Basic

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

perConnection
  :: ServerCxt t m
  => Dynamic t NS.Socket
  -> Event t ByteString
  -> m (Event t ByteString, Event t ())
perConnection dSocket eTx = mdo
  s <- sample . current $ dSocket
  so <- socket $ SocketConfig s 4096 eTx eQuit

  let
    eRx = _sReceive so
    eClosed = _sClose so
    eQuit = leftmost [void . ffilter (== "quit") $ eRx, eClosed]
    eOut = ffilter (/= "quit") eRx

  pure (eOut, eQuit)

go :: forall t m. BasicGuestConstraints t m => BasicGuest t m ((), Event t ())
go = mdo
  a <- accept $ AcceptConfig (Just "127.0.0.1") (Just "9000") 1 eQuit

  let eAccept = fst <$> _aAcceptSocket a
  eAdds :: Event t (Integer, NS.Socket) <- numberOccurrences eAccept

  -- Maintain a map of all open sockets, indexed by their occurrence number.
  dSocketMap <- foldDyn ($) Map.empty . mergeWith (.) $
    [ uncurry Map.insert <$> eAdds
    , flip (foldr Map.delete) <$> eRemoves
    ]

  dmeSocketEvents
    :: Dynamic t (Map Integer (Event t ByteString, Event t ()))
    <- list dSocketMap $ \dSocket -> perConnection dSocket eChats

  let
    eChats :: Event t ByteString
    eChats = dmeSocketEvents
      -- Pick out the "data received" events, and go from "dynamic map of
      -- events" to "dynamic of (event of map)"
      & fmap (mergeMap . fmap fst)

      -- listen to the latest Event
      & switchDyn

      -- Collect all the received data into one big ByteString
      & fmap (mconcat . Map.elems)

    eRemoves = dmeSocketEvents
      & fmap (mergeMap . fmap snd)
      & switchDyn
      & fmap Map.keys

    -- Quit if we end up with an empty map
    eQuitMap = void . ffilter id . updated $ Map.null <$> dSocketMap
    eQuitListenClose = _aClose a
    eQuitListenError = void $ _aError a
    eQuit = leftmost [eQuitMap, eQuitListenClose, eQuitListenError]
  pure ((), eQuit)

main :: IO ()
main = basicHostWithQuit go
