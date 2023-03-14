{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language KindSignatures #-}
{-# language RankNTypes #-}

module Slice
  ( Slice(..)
  , index
  , foldl'
  , ifoldl'
  , foldrMap
  ) where

import Array (R)
import Core (Slice(Slice))
import GHC.Exts (TYPE)
import qualified Core as C

index :: Slice a -> Int -> a
{-# inline index #-}
index (Slice v off _) ix = C.index v (off + ix)

foldl' :: (b -> a -> b) -> b -> Slice a -> b
{-# inline foldl' #-}
foldl' g !b0 (Slice v off0 len0) = go b0 off0 len0 where
  go !b !off !len
    | len == 0 = b
    | otherwise = go (g b (C.index v off)) (off + 1) (len - 1)

ifoldl' :: (b -> Int -> a -> b) -> b -> Slice a -> b
{-# inline ifoldl' #-}
ifoldl' g !b0 (Slice v off0 len0) = go b0 0 len0 where
  go !b !ix !len
    | len == 0 = b
    | otherwise = go (g b ix (C.index v (off0 + ix))) (ix + 1) (len - 1)

foldrMap :: Monoid m => (a -> m) -> Slice a -> m
{-# inline foldrMap #-}
foldrMap g (Slice v off0 len0) = go off0 len0 where
  go !off !len
    | len == 0 = mempty
    | otherwise = g (C.index v off) <> go (off + 1) (len - 1)
