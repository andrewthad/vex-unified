{-# language MagicHash #-}
{-# language RankNTypes #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeInType #-}
{-# language UnboxedTuples #-}

module Array
  ( R
  , A#
  , RA#
  , T
  , M#
  , RM#
  , Upcast
  , width
  , index#
  , write#
  , size#
  , unsafeFreeze#
  , uninitialized#
  , upcast
  , downcast
  , upcastMutable
  , downcastMutable
  ) where

import GHC.Exts
import Data.Kind (Type)
import Data.Primitive (Array(..),MutableArray(..))

type R = 'LiftedRep
type A# = Array#
type M# = MutableArray#

type T = Type
type RA# = Array#
type RM# = MutableArray#

width :: Int
width = 1

size# :: forall (a :: TYPE R). A# a -> Int#
size# = sizeofArray#

index# :: forall (a :: Type). Array# a -> Int# -> a
index# a i = case indexArray# a i of
  (# r #) -> r

write# :: forall (s :: Type) (a :: Type).
  MutableArray# s a -> Int# -> a -> State# s -> State# s
write# = writeArray#


unsafeFreeze# :: forall (s :: Type) (a :: TYPE R).
     M# s a
  -> State# s
  -> (# State# s, A# a #)
unsafeFreeze# = unsafeFreezeArray#

uninitialized# :: forall (s :: Type) (a :: TYPE R).
     Int#
  -> State# s
  -> (# State# s, M# s a #)
uninitialized# i s = newArray# i errorThunk s

errorThunk :: a
{-# noinline errorThunk #-}
errorThunk = error "Array: uninitialized element"

upcast :: forall (a :: TYPE R). A# a -> RA# (Upcast a)
upcast x = x

downcast :: forall (a :: TYPE R). RA# (Upcast a) -> A# a
downcast x = x

upcastMutable :: forall (s :: Type) (a :: TYPE R). M# s a -> RM# s (Upcast a)
upcastMutable x = x

downcastMutable :: forall (s :: Type) (a :: TYPE R). RM# s (Upcast a) -> M# s a
downcastMutable x = x

type family Upcast (a :: Type) :: Type where
  Upcast x = x
