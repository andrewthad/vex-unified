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
  , unlift
  , C.substitute
  , C.initialized
  , C.unsafeCoerceLength
    -- * Ranges
  , C.set
    -- * Freeze
  , C.unsafeShrinkFreeze
  , C.unsafeFreeze
    -- * Composite
  , map
  , ifoldl'
  , ifoldlSlice'
  ) where

import Prelude hiding (read,map)

import Core (Vector(..),Vector#,MutableVector(..),unsafeFreeze,index,write)
import Data.Unlifted (Maybe#(..))
import Element (R,A#,M#)
import GHC.Exts (Int(I#),RuntimeRep)
import GHC.ST (ST,runST)
import Data.Kind (Type)
import GHC.Exts (TYPE,State#,Int#,(*#))
import Arithmetic.Unsafe (Fin#(Fin#))
import Arithmetic.Types (type (<),Fin(Fin),Nat#)
import Arithmetic.Types (type (:=:),type (<=))
import GHC.TypeNats (type (+))

import qualified Element as A
import qualified Arithmetic.Equal as Equal
import qualified Arithmetic.Fin as Fin
import qualified Arithmetic.Plus as Plus
import qualified Arithmetic.Types as Arithmetic
import qualified Arithmetic.Lt as Lt
import qualified Arithmetic.Lte as Lte
import qualified Arithmetic.Nat as Nat
import qualified Core as C
import qualified GHC.TypeNats as GHC

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

-- | Map over a slice of a vector.
mapSlice :: forall (i :: GHC.Nat) (m :: GHC.Nat) (n :: GHC.Nat) (a :: TYPE R).
     (i + n <= m)
  -> (a -> a)
  -> Vector m a
  -> Nat# i -- start index
  -> Nat# n -- length
  -> Vector n a
{-# inline mapSlice #-}
mapSlice p f v off0 n = runST action where
  -- TODO: We should use Fin.ascendFromM_# to avoid unneeded additions.
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
