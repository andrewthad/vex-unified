{-# language GADTSyntax #-}
{-# language MagicHash #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeInType #-}
{-# language UnboxedTuples #-}
{-# language UnliftedNewtypes #-}

module Unlifted
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

type R = 'UnliftedRep
type A# = UnliftedArray#
type M# = MutableUnliftedArray#

type T = ()
type RA# = ConstArrayArray#
type RM# = ConstMutableArrayArray#

newtype ConstArrayArray# :: () -> TYPE 'UnliftedRep where
  ConstArrayArray# :: forall (a :: ()). ArrayArray# -> ConstArrayArray# a

newtype ConstMutableArrayArray# :: Type -> () -> TYPE 'UnliftedRep where
  ConstMutableArrayArray# :: forall (s :: Type) (a :: ()). MutableArrayArray# s -> ConstMutableArrayArray# s a

newtype UnliftedArray# :: TYPE 'UnliftedRep -> TYPE 'UnliftedRep where
  UnliftedArray# :: forall (a :: TYPE 'UnliftedRep). ArrayArray# -> UnliftedArray# a

newtype MutableUnliftedArray# :: Type -> TYPE 'UnliftedRep -> TYPE 'UnliftedRep where
  MutableUnliftedArray# :: forall (s :: Type) (a :: TYPE 'UnliftedRep). MutableArrayArray# s -> MutableUnliftedArray# s a

width :: Int
width = 1

size# :: forall (a :: ()). RA# a -> Int#
size# (ConstArrayArray# x) = sizeofArrayArray# x

index# :: forall (a :: TYPE 'UnliftedRep). UnliftedArray# a -> Int# -> a
index# (UnliftedArray# a) i = case indexByteArrayArray# a i of
  r -> (unsafeCoerce# :: ByteArray# -> a) r

write# :: forall (s :: Type) (a :: TYPE 'UnliftedRep).
  MutableUnliftedArray# s a -> Int# -> a -> State# s -> State# s
write# (MutableUnliftedArray# x) ix v = writeByteArrayArray# x ix
  ((unsafeCoerce# :: a -> ByteArray#) v)

unsafeFreeze# :: forall (s :: Type) (a :: ()).
     RM# s a
  -> State# s
  -> (# State# s, RA# a #)
unsafeFreeze# (ConstMutableArrayArray# x) s = case unsafeFreezeArrayArray# x s of
  (# s', y #) -> (# s', ConstArrayArray# y #)

uninitialized# :: forall (s :: Type) (a :: ()).
     Int#
  -> State# s
  -> (# State# s, RM# s a #)
uninitialized# i s = case newArrayArray# i s of
  (# s', x #) -> (# s', ConstMutableArrayArray# x #)

upcast :: forall (a :: TYPE 'UnliftedRep). A# a -> RA# (Upcast a)
upcast (UnliftedArray# x) = ConstArrayArray# x

downcast :: forall (a :: TYPE 'UnliftedRep). RA# (Upcast a) -> A# a
downcast (ConstArrayArray# x) = UnliftedArray# x

upcastMutable :: forall (s :: Type) (a :: TYPE 'UnliftedRep). M# s a -> RM# s (Upcast a)
upcastMutable (MutableUnliftedArray# x) = ConstMutableArrayArray# x

downcastMutable :: forall (s :: Type) (a :: TYPE 'UnliftedRep). RM# s (Upcast a) -> M# s a
downcastMutable (ConstMutableArrayArray# x) = MutableUnliftedArray# x

type family Upcast (a :: TYPE 'UnliftedRep) :: () where
  Upcast _ = '()

