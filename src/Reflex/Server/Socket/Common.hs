{-# LANGUAGE TemplateHaskell #-}
module Reflex.Server.Socket.Common (
    SocketConfig(..)
  , scInitSocket
  , scMaxRx
  , scSend
  , scClose
  , Socket(..)
  , sRecieve
  , sOpen
  , sError
  , sClosed
  ) where

import Control.Lens

import qualified Network.Socket as NS

import Reflex

data SocketConfig t a =
  SocketConfig {
    _scInitSocket :: NS.Socket
  , _scMaxRx      :: Int
  , _scSend       :: Event t [a]
  , _scClose      :: Event t ()
  }

makeLenses ''SocketConfig

data Socket t b =
  Socket {
    _sRecieve :: Event t b
  , _sOpen    :: Event t ()
  , _sError   :: Event t String
  , _sClosed  :: Event t ()
  }

makeLenses ''Socket

