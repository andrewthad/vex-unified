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
--
-- In this module, all functions have these properties:
--
-- * The unboxed variants of Fin and Nat are always used.
-- 
-- Functions with hash have these properties:
--
-- * The unboxed variants of Vector and MutableVector are used.
-- * The State# token is explicitly threaded through the function
--
-- Functions without hash have these properties:
--
-- * Boxed variants of Vector and MutableVector are used.
-- * ST is used, hiding the state token from the user.
--
-- Everything is arranged this way because the ST type constructor
-- requires a boxed argument, and the argument to ST is often
-- Vector or MutableVector. The only function left in a weird
-- position by this arrangement is @read@.
module Core
  ( -- Types
    Vector(..)
  , Vector#(..)
  , MutableVector(..)
  , MutableVector#
    -- * Primitive Functions
  , index#
  , index
  , write#
  , write
  -- , size
  -- , uninitialized
  , initialized
  , unsafeShrinkFreeze
  , set
  , unsafeFreeze
  , unsafeFreeze#
  , unsafeThaw
  , unsafeThaw#
  , unsafeCoerceLength
  , substitute
  ) where

import Prelude hiding (read,map)

import Arithmetic.Unsafe (Fin#(Fin#))
import Arithmetic.Unsafe (Nat#(Nat#))
import Element (R,A#,M#)
import Data.Kind (Type)
import GHC.Exts (Int(I#),RuntimeRep(IntRep,TupleRep,BoxedRep),Levity(Unlifted))
import GHC.Exts (TYPE,State#,Int#,(*#))
import GHC.ST (ST(ST),runST)
import GHC.TypeNats (type (+))
import Arithmetic.Types (type (:=:),type (<=))

import qualified Element as A
import qualified Arithmetic.Types as Arithmetic
import qualified GHC.TypeNats as GHC

data Vector :: GHC.Nat -> TYPE R -> Type where
  Vector :: Vector# n a -> Vector n a

newtype Vector# :: GHC.Nat -> TYPE R -> TYPE ('BoxedRep 'Unlifted) where
  Vector# :: A# a -> Vector# n a

data MutableVector :: Type -> GHC.Nat -> TYPE R -> Type where
  MutableVector :: MutableVector# s n a -> MutableVector s n a

newtype MutableVector# :: Type -> GHC.Nat -> TYPE R -> TYPE ('BoxedRep 'Unlifted) where
  MutableVector# :: M# s a -> MutableVector# s n a

set :: 
     (i + n <= m)
  -> MutableVector s n a
  -> Nat# i
  -> Nat# m
  -> a
  -> ST s ()
set _ (MutableVector (MutableVector# x)) (Nat# off) (Nat# len) a =
  ST $ \s -> case A.set# x off len a s of
    s' -> (# s', () #)

index# :: forall (n :: GHC.Nat) (a :: TYPE R).
     Vector# n a
  -> Fin# n
  -> a
index# (Vector# x) (Fin# i) = A.index# x i

index :: forall (n :: GHC.Nat) (a :: TYPE R).
     Vector n a
  -> Fin# n
  -> a
index (Vector x) i = index# x i

write# :: forall (s :: Type) (n :: GHC.Nat) (a :: TYPE R).
     MutableVector# s n a
  -> Fin# n
  -> a
  -> State# s
  -> State# s
{-# inline write# #-}
write# (MutableVector# x) (Fin# i) = A.write# x i

initialized :: forall (s :: Type) (n :: GHC.Nat) (a :: TYPE R).
     Nat# n
  -> a
  -> ST s (MutableVector s n a)
initialized !(Nat# n) a = ST $ \s0 -> case A.initialized# n a s0 of
  (# s1, x #) -> (# s1, MutableVector (MutableVector# x) #)

write :: forall (s :: Type) (n :: GHC.Nat) (a :: TYPE R).
     MutableVector s n a
  -> Fin# n -- index
  -> a
  -> ST s ()
write (MutableVector x) i a = ST \s ->
  (# write# x i a s, () #)

-- | The argument array must not be reused.
unsafeShrinkFreeze :: forall (s :: Type) (n0 :: GHC.Nat) (n1 :: GHC.Nat) (a :: TYPE R).
     (n1 <= n0)
  -> MutableVector s n0 a
  -> Nat# n1
  -> ST s (Vector n1 a)
unsafeShrinkFreeze _ (MutableVector (MutableVector# m)) (Nat# n) =
  ST \s -> case A.unsafeShrinkFreeze# m n s of
    (# s', y #) -> (# s', Vector (Vector# y) #)

unsafeFreeze :: forall (s :: Type) (n :: GHC.Nat) (a :: TYPE R).
  MutableVector s n a -> ST s (Vector n a)
unsafeFreeze (MutableVector (MutableVector# m)) =
  ST \s -> case A.unsafeFreeze# m s of
    (# s', y #) -> (# s', Vector (Vector# y) #)

unsafeFreeze# :: forall (s :: Type) (n :: GHC.Nat) (a :: TYPE R).
  MutableVector# s n a -> State# s -> (# State# s, Vector# n a #)
unsafeFreeze# (MutableVector# m) s0 =
  case A.unsafeFreeze# m s0 of
    (# s1, y #) -> (# s1, Vector# y #)

-- The word "unsafe" is used here in a very different sense than
-- ghc-prim uses it for this function. 
unsafeThaw# :: Vector# n a -> Int -> Int -> State# s -> (# State# s, MutableVector# s n a #)
unsafeThaw# (Vector# v) (I# off) (I# len) s0 =
  case A.thaw# v off len s0 of
    (# s1, mv #) -> (# s1, MutableVector# mv #)

unsafeThaw ::
     (i + n <= m)
  -> Vector m a
  -> Nat# i
  -> Nat# n
  -> ST s (MutableVector s n a)
unsafeThaw _ (Vector (Vector# v)) (Nat# off) (Nat# len) = ST $ \s0 ->
  case A.thaw# v off len s0 of
    (# s1, mv #) -> (# s1, MutableVector (MutableVector# mv) #)

substitute :: (m :=: n) -> Vector m a -> Vector n a
substitute !_ (Vector (Vector# x)) = Vector (Vector# x)

-- | Tell the type system that a vector has a certain length
--   without proving it.
unsafeCoerceLength :: Arithmetic.Nat n -> Vector m a -> Vector n a
unsafeCoerceLength !_ (Vector (Vector# x)) = Vector (Vector# x)

