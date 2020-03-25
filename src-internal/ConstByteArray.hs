{-# language DataKinds #-}
{-# language KindSignatures #-}
{-# language MagicHash #-}
{-# language RankNTypes #-}
{-# language TypeApplications #-}
{-# language UnboxedTuples #-}

module ConstByteArray
  ( RA#
  , RM#
  , T
  , unsafeFreeze#
  , size#
  , uninitialized#
  ) where

import Internal
import GHC.Exts
import Data.Kind (Type)

type T = ()
type RA# = ConstByteArray#
type RM# = ConstMutableByteArray#

unsafeFreeze# :: forall (s :: Type) (a :: ()).
     RM# s a
  -> State# s
  -> (# State# s, RA# a #)
unsafeFreeze# (ConstMutableByteArray# x) s = case unsafeFreezeByteArray# x s of
  (# s', y #) -> (# s', ConstByteArray# y #)

size# :: forall (a :: ()). RA# a -> Int#
size# (ConstByteArray# x) = sizeofByteArray# x

uninitialized# :: forall (s :: Type) (a :: ()).
     Int#
  -> State# s
  -> (# State# s, RM# s a #)
uninitialized# i s = case newByteArray# i s of
    (# s', x #) -> (# s', ConstMutableByteArray# x #)

