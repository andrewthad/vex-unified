{-# language BangPatterns #-}
{-# language BlockArguments #-}
{-# language DataKinds #-}
{-# language ExplicitNamespaces #-}
{-# language GADTSyntax #-}
{-# language KindSignatures #-}
{-# language MagicHash #-}
{-# language UnliftedNewtypes #-}
{-# language PolyKinds #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeOperators #-}
{-# language UnboxedTuples #-}

-- The only operatations defined in this module are those
-- that are considered primitive. That is, they cannot be
-- defined in terms of other operations on length-indexed
-- vectors.
module Core
  ( -- Types
    Vector(..)
  , Vector#
  , Slice(..)
  , Slice#
  , MutableVector(..)
  , MutableVector#
  , index#
  , index
  , write#
  , write
  , size
  , uninitialized
  , unsafeFreeze
  ) where

import Prelude hiding (read,map)

import Element (R,width,A#,M#)
import GHC.Exts (Int(I#),RuntimeRep(UnliftedRep,IntRep,TupleRep))
import GHC.ST (ST(ST),runST)
import Data.Kind (Type)
import GHC.Exts (TYPE,State#,Int#,(*#))

import qualified Element as E
import qualified Array as A

newtype Slice# :: TYPE R -> TYPE ('TupleRep '[ 'UnliftedRep,'IntRep,'IntRep]) where
  Slice# :: (# Vector# a, Int#, Int# #) -> Slice# a

data Slice :: TYPE R -> Type where
  Slice ::
       {-# UNPACK #-} !(Vector a)
    -> {-# UNPACK #-} !Int
    -> {-# UNPACK #-} !Int
    -> Slice a

data Vector :: TYPE R -> Type where
  Vector :: Vector# a -> Vector a

newtype Vector# :: TYPE R -> TYPE 'UnliftedRep where
  Vector# :: A# a -> Vector# a

data MutableVector :: Type -> TYPE R -> Type where
  MutableVector :: MutableVector# s a -> MutableVector s a

newtype MutableVector# :: Type -> TYPE R -> TYPE 'UnliftedRep where
  MutableVector# :: M# s a -> MutableVector# s a

unI :: Int -> Int#
unI (I# i) = i

uninitialized :: forall (s :: Type) (a :: TYPE R).
     Int
  -> ST s (MutableVector s a)
uninitialized (I# n) = ST \s -> case A.uninitialized# (n *# unI width) s of
  (# s', r #) -> (# s', MutableVector (MutableVector# (E.downcastMutable r)) #)

size :: forall (a :: TYPE R). Vector a -> Int
size (Vector (Vector# x)) = I# (A.size# (E.upcast x) *# unI width)

index# :: forall (a :: TYPE R).
     Vector# a
  -> Int#
  -> a
index# (Vector# x) i = E.index# x i

index :: forall (a :: TYPE R).
     Vector a
  -> Int
  -> a
index (Vector x) (I# i) = index# x i

write# :: forall (s :: Type) (a :: TYPE R).
     MutableVector# s a
  -> Int#
  -> a
  -> State# s
  -> State# s
write# (MutableVector# x) = E.write# x

write :: forall (s :: Type) (a :: TYPE R).
     MutableVector s a
  -> Int
  -> a
  -> ST s ()
write (MutableVector x) (I# i) a = ST \s ->
  (# write# x i a s, () #)

unsafeFreeze :: forall (s :: Type) (a :: TYPE R).
  MutableVector s a -> ST s (Vector a)
unsafeFreeze (MutableVector (MutableVector# m)) =
  ST \s -> case A.unsafeFreeze# (E.upcastMutable m) s of
    (# s', y #) -> (# s', Vector (Vector# (E.downcast y)) #)

map :: forall (a :: TYPE R). (a -> a) -> Vector a -> Vector a
{-# inline map #-}
map f src = runST do
  let sz = size src
  dst <- uninitialized sz
  let go !ix = case ix of
        (-1) -> pure ()
        _ -> do
          write dst ix (f (index src ix))
          go (ix - 1)
  go (sz - 1)
  unsafeFreeze dst

