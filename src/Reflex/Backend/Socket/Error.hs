{-|
Module      : Reflex.Backend.Socket.Error
Copyright   : (c) 2018-2019, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com, jack.kelly@data61.csiro.au
Stability   : experimental
Portability : non-portable
-}

{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE TemplateHaskell #-}

module Reflex.Backend.Socket.Error
  ( SetupError(..)

  -- * Prisms
  -- ** 'SetupError'
  , _GetAddrInfoError
  , _UseAddrInfoError
  ) where

import Control.Exception (IOException)
import Data.List.NonEmpty (NonEmpty)
import GHC.Generics (Generic)
import Network.Socket (AddrInfo)
import Control.Lens.TH (makePrisms)

-- | If a "socket setup" fails ('Reflex.Backend.Socket.Accept.accept'
-- or 'Reflex.Backend.Socket.Connect.connect'), you'll inspect one of
-- these to find out why.
data SetupError
  = GetAddrInfoError IOException
    -- ^ Call to 'Network.Socket.getAddrInfo' failed.
  | UseAddrInfoError (NonEmpty (AddrInfo, IOException))
    -- ^ We failed to set up a socket with any 'AddrInfo' we were
    -- given, and here are the corresponding exceptions each time we
    -- tried. For 'Reflex.Backend.Socket.Accept.accept', this means
    -- either 'Network.Socket.bind' or 'Network.Socket.listen' failed,
    -- and for 'Reflex.Backend.Socket.Connect.connect', this means
    -- 'Network.Socket.Connect' failed.
  deriving (Eq, Generic, Show)

$(makePrisms ''SetupError)
