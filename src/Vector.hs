{-# language BangPatterns #-}
{-# language BlockArguments #-}
{-# language DataKinds #-}
{-# language ExplicitNamespaces #-}
{-# language GADTSyntax #-}
{-# language KindSignatures #-}
{-# language MagicHash #-}
{-# language UnliftedNewtypes #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeOperators #-}
{-# language UnboxedTuples #-}

-- The only operatations defined in this module are those
-- that are considered primitive. That is, they cannot be
-- defined in terms of other operations on length-indexed
-- vectors.
module Vector
  ( -- Types
    C.Vector(..)
  , C.Vector#
  , C.MutableVector(..)
  , C.MutableVector#
    -- * Primitives
  , C.write#
  , C.write
  , C.index#
  , C.index
  , C.unsafeFreeze
  , unlift
  , C.substitute
  , C.initialized
  , C.unsafeCoerceLength
    -- * Composite
  , map
  , ifoldl'
  , ifoldlSlice'
  ) where

import Prelude hiding (read,map)

import Core (Vector(..),Vector#,MutableVector(..),unsafeFreeze,unsafeSet,index,write)
import Data.Unlifted (Maybe#(..))
import Array (R,A#,M#)
import GHC.Exts (Int(I#),RuntimeRep)
import GHC.ST (ST,runST)
import Data.Kind (Type)
import GHC.Exts (TYPE,State#,Int#,(*#))
import Arithmetic.Unsafe (Fin#(Fin#))
import Arithmetic.Types (type (<),Fin(Fin),Nat#)
import Arithmetic.Types (type (:=:),type (<=))
import GHC.TypeNats (type (+))

import qualified Array as A
import qualified Arithmetic.Equal as Equal
import qualified Arithmetic.Fin as Fin
import qualified Arithmetic.Plus as Plus
import qualified Arithmetic.Types as Arithmetic
import qualified Arithmetic.Lt as Lt
import qualified Arithmetic.Lte as Lte
import qualified Arithmetic.Nat as Nat
import qualified Core as C
import qualified GHC.TypeNats as GHC

-- unfoldrN :: Int -> (b -> Maybe# (# a, b #)) -> b -> Vector 'Unknown a
-- unfoldrN !n f !b0 = runST do
--   dst <- uninitialized n
--   let go !ix !b
--         | ix == n = unsafeFreeze dst
--         | Maybe# (# | (# a, b' #) #) <- f b = unsafeWrite dst ix a *> go (ix + 1) b'
--         | otherwise = C.unsafeShrink dst ix
--   go 0 b0

ifoldlSlice' :: forall (i :: GHC.Nat) (m :: GHC.Nat) (n :: GHC.Nat) (a :: TYPE R) (b :: Type).
     (i + n <= m)
  -> (b -> Fin# (i + n) -> a -> b)
  -> b
  -> Vector m a
  -> Nat# i
  -> Nat# n
  -> b
{-# inline ifoldlSlice' #-}
ifoldlSlice' p f b0 v off0 n =
  Fin.ascendFrom'# off0 n b0 $ \fin b ->
    let callback :: forall (j :: GHC.Nat). (j < i + n) -> Nat# j -> b
        callback lt ix = case C.index v (Fin.construct# (Lt.transitiveNonstrictR lt p) ix) of
          a0 -> f b fin a0
     in Fin.with# fin callback

ifoldl' :: forall (n :: GHC.Nat) (a :: TYPE R) (b :: Type).
     (b -> Fin# n -> a -> b)
  -> b
  -> Vector n a
  -> Nat# n
  -> b
{-# inline ifoldl' #-}
ifoldl' f b0 v n = ifoldlSlice' (Lte.reflexive @n) f b0 v (Nat.zero# (# #)) n

-- x go b0 off0 len0 where
-- x   go !b !off !len
-- x     | len == 0 = b
-- x     | otherwise = go (g b (C.unsafeIndex v off)) (off + 1) (len - 1)

-- x ifoldl' :: (b -> Fin n -> a -> b) -> b -> Slice ('Known n) a -> b
-- x {-# inline ifoldl' #-}
-- x ifoldl' g !b0 (Slice v off0 len0) = go b0 0 len0 where
-- x   go !b !ix !len
-- x     | len == 0 = b
-- x     | otherwise =
-- x         let fin = Fin (Unsafe.Nat ix) Unsafe.Lt
-- x          in go (g b fin (C.unsafeIndex v (off0 + ix))) (ix + 1) (len - 1)
-- x 
-- x foldrMap :: Monoid m => (a -> m) -> Slice n a -> m
-- x {-# inline foldrMap #-}
-- x foldrMap g (Slice v off0 len0) = go off0 len0 where
-- x   go !off !len
-- x     | len == 0 = mempty
-- x     | otherwise = g (C.index v off) <> go (off + 1) (len - 1)

-- | Map over a vector starting at an offset.
mapSlice :: forall (i :: GHC.Nat) (m :: GHC.Nat) (n :: GHC.Nat) (a :: TYPE R).
     (i + n <= m)
  -> (a -> a)
  -> Vector m a
  -> Nat# i -- start index
  -> Nat# n -- length
  -> Vector n a
{-# inline mapSlice #-}
mapSlice p f v off0 n = runST action where
  action :: forall s. ST s (Vector n a)
  action = do
    dst <- C.unsafeThaw p v off0 n
    Fin.ascendM_# n $ \fin -> do
      let callback :: forall (j :: GHC.Nat). (j < n) -> Nat# j -> ST s ()
          callback lt ix = case C.index v (Fin.construct# (Lt.decrementR @n (Lt.substituteL (Equal.symmetric (Plus.associative @j @i @n)) (Lt.substituteR (Plus.commutative @n @m) (Lt.plus lt p)))) (Nat.plus# ix off0)) of
            a0 -> C.write dst fin (f a0)
      Fin.with# fin callback
    C.unsafeFreeze dst

-- | Map over a vector starting at offset 0.
map :: 
     (a -> a)
  -> Vector n a
  -> Nat# n -- length
  -> Vector n a
{-# inline map #-}
map f v n = mapSlice Lte.reflexive f v (Nat.zero# (# #)) n

unlift :: Vector n a -> Vector# n a
unlift (Vector x) = x

