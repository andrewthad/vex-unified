{-# language MagicHash #-}
{-# language RankNTypes #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeInType #-}
{-# language StandaloneKindSignatures #-}
{-# language UnboxedTuples #-}

module Word8
  ( R
  , A#
  , ArrayRep
  , M#
  , index#
  , write#
  , unsafeFreeze#
  , initialized#
  , set#
  , shrink#
  , thaw#
  ) where

import GHC.Exts
import Data.Kind (Type)
import PrimArray (PrimArray#(..),MutablePrimArray#(..))

import qualified GHC.Exts as Exts

type ArrayRep = 'BoxedRep 'Unlifted
type A# = PrimArray# @'Word8Rep
type M# = MutablePrimArray# @'Word8Rep
type R = 'Word8Rep

unsafeFromW8 :: forall (a :: TYPE 'Word8Rep). Word8# -> a
unsafeFromW8 x = unsafeCoerce# x

unsafeToW8 :: forall (a :: TYPE 'Word8Rep). a -> Word8#
unsafeToW8 x = unsafeCoerce# x

index# :: forall (a :: TYPE R). A# a -> Int# -> a
index# (PrimArray# a) i = unsafeFromW8 (indexWord8Array# a i)

write# :: forall (s :: Type) (a :: TYPE R).
  M# s a -> Int# -> a -> State# s -> State# s
write# (MutablePrimArray# m) ix a s = writeWord8Array# m ix (unsafeToW8 a) s

unsafeFreeze# :: forall (s :: Type) (a :: TYPE R).
     M# s a
  -> State# s
  -> (# State# s, A# a #)
unsafeFreeze# (MutablePrimArray# m) s0 = case unsafeFreezeByteArray# m s0 of
  (# s1, v #) -> (# s1, PrimArray# v #)

initialized# :: forall (s :: Type) (a :: TYPE R).
     Int#
  -> a
  -> State# s
  -> (# State# s, M# s a #)
initialized# n a s0 = case newByteArray# n s0 of
  (# s1, m #) -> case Exts.setByteArray# m 0# n (Exts.word2Int# (Exts.word8ToWord# (unsafeToW8 a))) s1 of
    s2 -> (# s2, MutablePrimArray# m #)

set# :: forall (s :: Type) (a :: TYPE R).
     M# s a
  -> Int#
  -> Int#
  -> a
  -> State# s
  -> State# s
set# (MutablePrimArray# m) off0 len0 a s0 = Exts.setByteArray# m off0 len0 (Exts.word2Int# (Exts.word8ToWord# (unsafeToW8 a))) s0

-- shrink and freeze, all at once
shrink# ::
     M# s a
  -> Int#
  -> State# s
  -> (# State# s, A# a #)
shrink# (MutablePrimArray# m) i s0Alpha = case getSizeofMutableByteArray# m s0Alpha of
  (# s0, sz #) -> case sz ==# i of
    1# -> case Exts.unsafeFreezeByteArray# m s0 of
      (# s1, v #) -> (# s1, PrimArray# v #)
    _ -> case Exts.shrinkMutableByteArray# m i s0 of
      s1 -> case Exts.unsafeFreezeByteArray# m s1 of
        (# s2, v #) -> (# s2, PrimArray# v #)

thaw# :: forall (s :: Type) (a :: TYPE R).
     A# a
  -> Int#
  -> Int#
  -> State# s
  -> (# State# s, M# s a #)
thaw# (PrimArray# v) off len s0 = case Exts.newByteArray# len s0 of
  (# s1, m #) -> case Exts.copyByteArray# v off m 0# len s1 of
    s2 -> (# s2, MutablePrimArray# m #)
