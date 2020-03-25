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
  , C.index#
  , C.index
  , C.write#
  , C.write
  , C.size
  , C.uninitialized
    -- * Composite
  , map
  ) where

import Prelude hiding (read,map)

import Core (Vector(Vector),unsafeFreeze,uninitialized,index,write,size)
import Element (R,width,A#,M#)
import GHC.Exts (Int(I#),RuntimeRep(UnliftedRep))
import GHC.ST (ST(ST),runST)
import Data.Kind (Type)
import GHC.Exts (TYPE,State#,Int#,(*#))

import qualified Core as C

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
