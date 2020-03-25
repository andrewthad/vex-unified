{-# language MagicHash #-}
{-# language RankNTypes #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeInType #-}

module Word8
  ( R
  , A#
  , M#
  , Upcast
  , upcast
  , downcast
  , upcastMutable
  , downcastMutable
  , width
  , upcast#
  , index#
  , write#
  ) where

import GHC.Exts
import Data.Kind (Type)
import Internal (PrimArray#(..),MutablePrimArray#(..))
import Internal (ConstByteArray#(..),ConstMutableByteArray#(..))
import ConstByteArray (RM#,RA#)

type A# = PrimArray# @'Word8Rep
type M# = MutablePrimArray# @'Word8Rep

type R = 'Word8Rep

type family Upcast (a :: TYPE 'Word8Rep) :: () where
  Upcast _ = '()

upcast :: forall (a :: TYPE R). A# a -> RA# (Upcast a)
upcast (PrimArray# x) = ConstByteArray# x

downcast :: forall (a :: TYPE R). RA# (Upcast a) -> A# a
downcast (ConstByteArray# x) = PrimArray# x

upcastMutable :: forall (s :: Type) (a :: TYPE R). M# s a -> RM# s (Upcast a)
upcastMutable (MutablePrimArray# x) = ConstMutableByteArray# x

downcastMutable :: forall (s :: Type) (a :: TYPE R). RM# s (Upcast a) -> M# s a
downcastMutable (ConstMutableByteArray# x) = MutablePrimArray# x

width :: Int
width = 1

upcast# :: forall (a :: TYPE 'Word8Rep). PrimArray# a -> ByteArray#
upcast# (PrimArray# x) = x

index# :: forall (a :: TYPE 'Word8Rep). PrimArray# a -> Int# -> a
index# (PrimArray# a) i = unsafeCoerce# (narrowWord8# (indexWord8Array# a i))

write# :: forall (s :: Type) (a :: TYPE 'Word8Rep).
  MutablePrimArray# s a -> Int# -> a -> State# s -> State# s
write# (MutablePrimArray# a) i w = writeWord8Array# a i
  (extendWord8# (unsafeCoerce# w))
