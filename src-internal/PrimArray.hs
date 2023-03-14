{-# language TypeInType #-}
{-# language GADTs #-}
{-# language TypeFamilies #-}
{-# language RankNTypes #-}
{-# language TypeApplications #-}
{-# language MagicHash #-}
{-# language UnliftedNewtypes #-}

module PrimArray
  ( PrimArray(..)
  , PrimArray#(..)
  , MutablePrimArray(..)
  , MutablePrimArray#(..)
  ) where

import GHC.Exts
import Data.Kind (Type)
import GHC.TypeNats (Nat)

data PrimArray :: forall (r :: RuntimeRep). TYPE r -> Type where
  PrimArray :: forall (r :: RuntimeRep) (a :: TYPE r). PrimArray# @r a -> PrimArray @r a

newtype PrimArray# :: forall (r :: RuntimeRep). TYPE r -> TYPE ('BoxedRep 'Unlifted) where
  PrimArray# :: forall (r :: RuntimeRep) (a :: TYPE r). ByteArray# -> PrimArray# a


newtype MutablePrimArray# :: forall (r :: RuntimeRep). Type -> TYPE r -> TYPE ('BoxedRep 'Unlifted) where
  MutablePrimArray# :: forall (r :: RuntimeRep) (s :: Type) (a :: TYPE r). MutableByteArray# s -> MutablePrimArray# s a

data MutablePrimArray :: forall (r :: RuntimeRep). Type -> TYPE r -> Type where
  MutablePrimArray :: forall (r :: RuntimeRep) (s :: Type) (a :: TYPE r). MutablePrimArray# s a -> MutablePrimArray s a


