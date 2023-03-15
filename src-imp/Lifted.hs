{-# language MagicHash #-}
{-# language RankNTypes #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeInType #-}
{-# language StandaloneKindSignatures #-}
{-# language UnboxedTuples #-}

module Lifted
  ( R
  , A#
  , ArrayRep
  , M#
  , index#
  , write#
  , unsafeFreeze#
  , uninitialized#
  , initialized#
  , set#
  , unsafeShrinkFreeze#
  , thaw#
  ) where

import GHC.Exts
import Data.Kind (Type)
import Data.Primitive (Array(..),MutableArray(..))

import qualified GHC.Exts as Exts

type ArrayRep = 'BoxedRep 'Unlifted
type R = 'BoxedRep 'Lifted

type A# :: TYPE ('BoxedRep 'Lifted) -> TYPE ('BoxedRep 'Unlifted)
type A# = Array#

type M# :: Type -> TYPE ('BoxedRep 'Lifted) -> TYPE ('BoxedRep 'Unlifted)
type M# = MutableArray#

index# :: forall (a :: TYPE R). A# a -> Int# -> a
index# a i = case indexArray# a i of
  (# r #) -> r

write# :: forall (s :: Type) (a :: TYPE R).
  M# s a -> Int# -> a -> State# s -> State# s
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

initialized# :: forall (s :: Type) (a :: TYPE R).
     Int#
  -> a
  -> State# s
  -> (# State# s, M# s a #)
initialized# i a s = newArray# i a s

set# :: forall (s :: Type) (a :: TYPE R).
     M# s a
  -> Int#
  -> Int#
  -> a
  -> State# s
  -> State# s
set# m off0 len0 a s0 =
  let go off len s = case len of
        0# -> s
        _ -> go (off +# 1#) (len -# 1#) (write# m off a s)
   in go off0 len0 s0

-- shrink and freeze, all at once
unsafeShrinkFreeze# ::
     M# s a
  -> Int#
  -> State# s
  -> (# State# s, A# a #)
unsafeShrinkFreeze# m i s0 = case sizeofMutableArray# m ==# i of
  1# -> Exts.unsafeFreezeArray# m s0
  _ -> Exts.freezeArray# m 0# i s0

-- makes a copy, does not alias the argument
thaw# :: forall (s :: Type) (a :: TYPE R).
     A# a
  -> Int#
  -> Int#
  -> State# s
  -> (# State# s, M# s a #)
thaw# = Exts.thawArray#

errorThunk :: a
{-# noinline errorThunk #-}
errorThunk = error "Array: uninitialized element"
