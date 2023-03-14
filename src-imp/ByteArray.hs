{-# language MagicHash #-}
{-# language RankNTypes #-}
{-# language UnboxedTuples #-}
{-# language TypeInType #-}
{-# language DataKinds #-}

module ByteArray
  ( UAR
  , UA
  , MUA
  , size#
  , unsafeFreeze#
  , uninitialized#
  ) where

import GHC.Exts
import Data.Kind

type UAR = 'BoxedRep 'Unlifted
type UA = ByteArray#
type MUA = MutableByteArray#

size# :: ByteArray# -> Int#
size# = sizeofByteArray#

unsafeFreeze# :: MutableByteArray# s -> State# s -> (# State# s, ByteArray# #)
unsafeFreeze# = unsafeFreezeByteArray#

uninitialized# :: Int# -> State# s -> (# State# s, MutableByteArray# s #)
uninitialized# = newByteArray#
