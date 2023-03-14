{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language UnliftedNewtypes #-}
{-# language KindSignatures #-}
{-# language GADTSyntax #-}
{-# language MagicHash #-}
{-# language RankNTypes #-}
{-# language UnboxedTuples #-}
{-# language TypeOperators #-}

module Vex.Sized.Vector
  ( -- * Types
    Vector(..)
  , Vector#
  , MutableVector(..)
  , MutableVector#
  , Slice(..)
    -- * Functions
  , index
  , write
  , unsafeFreeze
  , initialized
  , forget
  , with
  , substitute
    -- * Derived Functions
  , map
  , foldrMap
  , ifoldl'
  ) where

import Prelude hiding (map)

import Array (ArrayRep)

import Arithmetic.Types (Fin(Fin))
import Arithmetic.Types (type (:=:))
import Data.Kind (Type)
import Array (R)
import GHC.Exts (Int(I#),TYPE,RuntimeRep(UnliftedRep))
import GHC.ST (ST(ST))

import qualified Arithmetic.Unsafe as Unsafe
import qualified Arithmetic.Fin as Fin
import qualified Arithmetic.Nat as Nat
import qualified Arithmetic.Types as Arithmetic
import qualified GHC.TypeNats as GHC
import qualified Core as C
import qualified Vector as V
import qualified Slice

newtype Vector# :: GHC.Nat -> TYPE R -> TYPE ArrayRep where
  Vector# :: C.Vector# a -> Vector# n a

data Vector :: GHC.Nat -> TYPE R -> Type where
  Vector :: Vector# n a -> Vector n a

newtype MutableVector# :: Type -> GHC.Nat -> TYPE R -> TYPE ArrayRep where
  MutableVector# :: C.MutableVector# s a -> MutableVector# s n a

data MutableVector :: Type -> GHC.Nat -> TYPE R -> Type where
  MutableVector :: MutableVector# s n a -> MutableVector s n a

data Slice :: GHC.Nat -> TYPE R -> Type where
  Slice ::
       {-# UNPACK #-} !(Vector n a)
    -> {-# UNPACK #-} !Int -- offset, must be in bounds, not checked
    -> {-# UNPACK #-} !(Arithmetic.Nat n) -- length
    -> Slice n a

index :: forall (n :: GHC.Nat) (a :: TYPE R).
     Vector n a
  -> Fin n
  -> a
index (Vector (Vector# x)) (Fin ix !_) =
  case Nat.demote ix of
    I# i -> C.index# x i

write :: forall (n :: GHC.Nat) (s :: Type) (a :: TYPE R).
     MutableVector s n a
  -> Fin n
  -> a
  -> ST s ()
write (MutableVector (MutableVector# x)) (Fin ix !_) a =
  case Nat.demote ix of
    I# i -> ST (\s -> (# C.write# x i a s, () #) )

foldrMap :: Monoid m
  => Arithmetic.Nat n
  -> (a -> m)
  -> Vector n a
  -> m
{-# inline foldrMap #-}
foldrMap !n g (Vector (Vector# v)) = Slice.foldrMap g
  (C.Slice (C.Vector v) 0 (Nat.demote n))

ifoldl' :: Arithmetic.Nat n -> (b -> Fin n -> a -> b) -> b -> Vector n a -> b
{-# inline ifoldl' #-}
ifoldl' !n f !b0 !v = Fin.ascend' n b0
  (\fin b -> f b fin (index v fin))

initialized :: Arithmetic.Nat n -> a -> ST s (MutableVector s n a)
initialized !n a = do
  V.MutableVector mv <- V.initialized (Nat.demote n) a
  pure (MutableVector (MutableVector# mv))

unsafeFreeze :: MutableVector s n a -> ST s (Vector n a)
unsafeFreeze (MutableVector (MutableVector# x)) = ST $ \s0 ->
  case C.unsafeFreeze# x s0 of
    (# s1, y #) -> (# s1, Vector (Vector# y) #)

-- | Discard the phantom length associated with an indexed vector.
forget :: Vector n a -> C.Vector a
{-# INLINE forget #-}
forget (Vector (Vector# arr)) = C.Vector arr

with ::
     Int -- ^ Length of vector, unchecked
  -> C.Vector a -- ^ Vector
  -> (forall n. Arithmetic.Nat n -> Vector n a -> b)
  -> b
with !n (C.Vector x) f = f (Unsafe.Nat n) (Vector (Vector# x))

substitute :: (m :=: n) -> Vector m a -> Vector n a
substitute !_ (Vector (Vector# x)) = Vector (Vector# x)

