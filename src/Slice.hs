{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language KindSignatures #-}
{-# language RankNTypes #-}

module Slice
  ( index
  , foldl'
  ) where

import Element (R)
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
