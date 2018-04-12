{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Reflex.Server.Socket.Subhost (
    SocketConfig(..)
  , Socket(..)
  , socketSubhost
  ) where

import Data.Functor.Identity

import qualified Network.Socket as NS

import Data.GADT.Compare
import Data.Dependent.Map

import Reflex
import Reflex.Host.Basic
import Reflex.Host.Subhost

import Reflex.Server.Socket

data SocketConfigTag a i x where
  SCTSend  :: SocketConfigTag a i [a]
  SCTClose :: SocketConfigTag  a i ()
  SCTOther :: SocketConfigTag  a i i

instance GEq (SocketConfigTag a i) where
  geq SCTSend SCTSend = Just Refl
  geq SCTClose SCTClose = Just Refl
  geq SCTOther SCTOther = Just Refl
  geq _ _ = Nothing

instance GCompare (SocketConfigTag a i) where
  gcompare SCTSend SCTSend = GEQ
  gcompare SCTSend _ = GLT
  gcompare _ SCTSend = GGT
  gcompare SCTClose SCTClose = GEQ
  gcompare SCTClose _ = GLT
  gcompare _ SCTClose = GGT
  gcompare SCTOther SCTOther = GEQ

inToDMap :: Reflex t => SocketConfig t b -> Event t c -> (NS.Socket, Int, Event t (DMap (SocketConfigTag b c) Identity))
inToDMap (SocketConfig s i eSend eClose) eb =
  (s, i, merge $ fromList [SCTSend :=> eSend, SCTClose :=> eClose, SCTOther :=> eb])

dmapToIn :: Reflex t => NS.Socket -> Int -> Event t (DMap (SocketConfigTag b c) Identity) -> (SocketConfig t b, Event t c)
dmapToIn s i d =
  let
    f = fan d
  in
  (SocketConfig s i (select f SCTSend) (select f SCTClose), select f SCTOther)

data SocketTag b o x where
  STReceive :: SocketTag b o b
  STOpen    :: SocketTag b o ()
  STError   :: SocketTag b o String
  STClosed  :: SocketTag b o ()
  STOther   :: SocketTag b o o

instance GEq (SocketTag b o) where
  geq STReceive STReceive = Just Refl
  geq STOpen STOpen = Just Refl
  geq STError STError = Just Refl
  geq STClosed STClosed = Just Refl
  geq STOther STOther = Just Refl
  geq _ _ = Nothing

instance GCompare (SocketTag b o) where
  gcompare STReceive STReceive = GEQ
  gcompare STReceive _ = GLT
  gcompare _ STReceive = GGT
  gcompare STOpen STOpen = GEQ
  gcompare STOpen _ = GLT
  gcompare _ STOpen = GGT
  gcompare STError STError = GEQ
  gcompare STError _ = GLT
  gcompare _ STError = GGT
  gcompare STClosed STClosed = GEQ
  gcompare STClosed _ = GLT
  gcompare _ STClosed = GGT
  gcompare STOther STOther = GEQ

outToDMap :: Reflex t => Socket t d -> Event t e -> Event t (DMap (SocketTag d e) Identity)
outToDMap (Socket r o e c) i =
  merge $ fromList [STOther :=> i, STReceive :=> r, STOpen :=> o, STError :=> e, STClosed :=> c]

dmapToOut :: Reflex t => Event t (DMap (SocketTag d e) Identity) -> (Socket t d, Event t e)
dmapToOut e = let
    f = fan e
  in
    (Socket (select f STReceive) (select f STOpen) (select f STError) (select f STClosed), select f STOther)

transform :: NS.Socket
          -> Int
          -> (SocketConfig t a -> Dynamic t b -> Event t c -> BasicGuest t m (Socket t d, Event t e, Event t ()))
          -> Dynamic t b
          -> Event t (DMap (SocketConfigTag a c) Identity)
          -> BasicGuest t m (Event t (DMap (SocketTag d e) Identity), Event t ())
transform sock i fn d em = do
  let (sc, eIn) = dmapToIn sock i em
  (s, eOut, eQuit) <- fn sc d eIn
  pure $ (outToDMap s eOut, eQuit)

socketSubhost :: SocketConfig t a
              -> Dynamic t b
              -> Event t c
              -> (forall t1 m1. SocketConfig t1 a -> Dynamic t1 b -> Event t1 c -> BasicGuest t1 m1 (Socket t1 d, Event t1 e, Event t1 ()))
              -> BasicGuest t m (Socket t d, Event t e, Event t ())
socketSubhost sc da eb fn = do
  let (s, i, eIn) = inToDMap sc eb
  (eOut, eQuit) <- subhost da eIn $ transform s i fn
  let (sOut, eOut') = dmapToOut eOut
  pure $ (sOut, eOut', eQuit)
