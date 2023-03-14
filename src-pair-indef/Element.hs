{-# language DataKinds #-}
{-# language KindSignatures #-}
{-# language MagicHash #-}
{-# language RankNTypes #-}
{-# language TypeFamilies #-}
{-# language UnboxedTuples #-}

module Element
  ( R, A#, M#, Upcast
  , ElementA.width, downcast, upcast, downcastMutable, upcastMutable
  , index#, write#, set#
  ) where

import GHC.Exts (State#,Int#,RuntimeRep)
import GHC.Exts (TYPE,RuntimeRep)
import Data.Kind (Type)

import Array (T,RA#,RM#,ArrayRep)

import qualified ElementA
import qualified ElementB

type R = 'TupleRep '[ElementA.R, ElementB.R]

type A# = RA#
data M# = RM#

type family Upcast (a :: TYPE R) :: T where
  Upcast 

upcast :: forall (a :: TYPE R). A# a -> RA# (Upcast a)
upcast x = x

downcast :: forall (a :: TYPE R). RA# (Upcast a) -> A# a
downcast x = x

upcastMutable :: forall (s :: Type) (a :: TYPE R). M# s a -> RM# s (Upcast a)
upcastMutable x = x

downcastMutable :: forall (s :: Type) (a :: TYPE R). RM# s (Upcast a) -> M# s a
downcastMutable x = x

