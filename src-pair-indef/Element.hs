{-# language DataKinds #-}
{-# language BangPatterns #-}
{-# language UnliftedDatatypes #-}
{-# language ExistentialQuantification #-}
{-# language GADTSyntax #-}
{-# language UnliftedNewtypes #-}
{-# language KindSignatures #-}
{-# language MagicHash #-}
{-# language RankNTypes #-}
{-# language UnboxedTuples #-}
{-# language TypeFamilies #-}

-- Note: There are a lot of places where we write
--
-- > case unsafeToTuple e of
-- >   (# a,b #) -> ...
--
-- If we instead write
--
-- > let !(# a,b #) = unsafeToTuple e
--
-- GHC fails with a compiler panic.
module Element
  ( R
  , A#
  , M#
  , index#
  , write#
  , unsafeFreeze#
  , initialized#
  , set#
  , unsafeShrinkFreeze#
  , thaw#
  ) where

import Data.Kind (Type)
import GHC.Exts (TYPE,Levity(Unlifted),RuntimeRep(BoxedRep,TupleRep),State#,Int#,unsafeCoerce#)
import qualified ElementA as A
import qualified ElementB as B

type R = 'TupleRep '[A.R, B.R]

-- Warning: Using this data constructor directly is dangerous.
-- Most functions in this module use unsafe coerce to go between
-- type r and type (#a,b#). If a or b is a lifted type, it is
-- important that the matching component of r agrees with this type.
-- If not, you end up with segfaults.
data A# :: TYPE R -> TYPE ('BoxedRep 'Unlifted) where
  A# :: forall (a :: TYPE A.R) (b :: TYPE B.R) (r :: TYPE R).
        A.A# a
     -> B.A# b
     -> A# r

data M# :: Type -> TYPE R -> TYPE ('BoxedRep 'Unlifted) where
  M# :: forall (s :: Type) (a :: TYPE A.R) (b :: TYPE B.R) (r :: TYPE R).
        A.M# s a
     -> B.M# s b
     -> M# s r

unsafeFromTuple ::
  forall (a :: TYPE A.R) (b :: TYPE B.R) (x :: TYPE ('TupleRep '[A.R, B.R])).
  (# a, b #) -> x
unsafeFromTuple x = unsafeCoerce# x

unsafeToTuple ::
  forall (a :: TYPE A.R) (b :: TYPE B.R) (x :: TYPE ('TupleRep '[A.R, B.R])).
  x -> (# a, b #)
unsafeToTuple x = unsafeCoerce# x

initialized# :: forall (s :: Type) (a :: TYPE R).
     Int#
  -> a
  -> State# s
  -> (# State# s, M# s a #)
initialized# n e s0 = case unsafeToTuple e of
  (# a, b #) -> case A.initialized# n a s0 of
    (# s1, av #) -> case B.initialized# n b s1 of
      (# s2, bv #) -> (# s2, M# av bv #)

unsafeFreeze# :: forall (s :: Type) (a :: TYPE R).
     M# s a
  -> State# s
  -> (# State# s, A# a #)
unsafeFreeze# (M# a b) s0 = case A.unsafeFreeze# a s0 of
  (# s1, a' #) -> case B.unsafeFreeze# b s1 of
    (# s2, b' #) -> (# s2, A# a' b' #)

unsafeShrinkFreeze# :: forall (s :: Type) (a :: TYPE R).
     M# s a
  -> Int#
  -> State# s
  -> (# State# s, A# a #)
unsafeShrinkFreeze# (M# a b) n s0 = case A.unsafeShrinkFreeze# a n s0 of
  (# s1, a' #) -> case B.unsafeShrinkFreeze# b n s1 of
    (# s2, b' #) -> (# s2, A# a' b' #)

thaw# :: forall (s :: Type) (a :: TYPE R).
     A# a
  -> Int#
  -> Int#
  -> State# s
  -> (# State# s, M# s a #)
thaw# (A# a b) off len s0 = case A.thaw# a off len s0 of
  (# s1, a' #) -> case B.thaw# b off len s1 of
    (# s2, b' #) -> (# s2, M# a' b' #)

index# :: forall (a :: TYPE R).
     A# a
  -> Int#
  -> a
index# (A# x y) ix =
  unsafeFromTuple (# A.index# x ix, B.index# y ix #)

write# :: forall (s :: Type) (a :: TYPE R).
  M# s a -> Int# -> a -> State# s -> State# s
write# (M# dstA dstB) ix e s0 = case unsafeToTuple e of
  (# a, b #) -> case A.write# dstA ix a s0 of
    s1 -> B.write# dstB ix b s1

set# :: forall (s :: Type) (a :: TYPE R).
     M# s a
  -> Int#
  -> Int#
  -> a
  -> State# s
  -> State# s
set# (M# dstA dstB) off0 len0 e s0 = case unsafeToTuple e of
  (# a, b #) -> case A.set# dstA off0 len0 a s0 of
    s1 -> B.set# dstB off0 len0 b s1
