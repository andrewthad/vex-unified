{-# language TypeInType #-}
{-# language GADTs #-}
{-# language TypeFamilies #-}
{-# language RankNTypes #-}
{-# language TypeApplications #-}
{-# language MagicHash #-}
{-# language UnliftedNewtypes #-}

module Internal
  ( MutablePrimArray#(..)
  , MutablePrimArray(..)
  , PrimArray#(..)
  , PrimArray(..)
  -- , MutableVector#(..)
  -- , MutableVector(..)
  , ConstByteArray#(..)
  , ConstMutableByteArray#(..)
  -- , MutableRepresentation
  -- , Representation
  ) where

import GHC.Exts
import Data.Kind (Type)

newtype PrimArray# :: forall (r :: RuntimeRep). TYPE r -> TYPE 'UnliftedRep where
  PrimArray# :: forall (r :: RuntimeRep) (a :: TYPE r). ByteArray# -> PrimArray# a

data PrimArray :: forall (r :: RuntimeRep). TYPE r -> Type where
  PrimArray :: forall (r :: RuntimeRep) (a :: TYPE r). PrimArray# @r a -> PrimArray @r a

newtype ConstByteArray# :: () -> TYPE 'UnliftedRep where
  ConstByteArray# :: forall (a :: ()). ByteArray# -> ConstByteArray# a

newtype ConstMutableByteArray# :: Type -> () -> TYPE 'UnliftedRep where
  ConstMutableByteArray# :: forall (s :: Type) (a :: ()). MutableByteArray# s -> ConstMutableByteArray# s a

newtype MutablePrimArray# :: forall (r :: RuntimeRep). Type -> TYPE r -> TYPE 'UnliftedRep where
  MutablePrimArray# :: forall (r :: RuntimeRep) (s :: Type) (a :: TYPE r). MutableByteArray# s -> MutablePrimArray# s a

data MutablePrimArray :: forall (r :: RuntimeRep). Type -> TYPE r -> Type where
  MutablePrimArray :: forall (r :: RuntimeRep) (s :: Type) (a :: TYPE r). MutablePrimArray# s a -> MutablePrimArray s a

-- newtype Vector# :: forall (r :: RuntimeRep). TYPE r -> TYPE 'UnliftedRep where
--   Vector# :: Representation r a -> Vector# @r a
-- 
-- newtype MutableVector# :: forall (r :: RuntimeRep). Type -> TYPE r -> TYPE 'UnliftedRep where
--   MutableVector# :: MutableRepresentation r s a -> MutableVector# @r s a
-- 
-- data Vector :: forall (r :: RuntimeRep). TYPE r -> Type where
--   Vector :: forall (r :: RuntimeRep) (a :: TYPE r). Vector# @r a -> Vector @r a
-- 
-- data MutableVector :: forall (r :: RuntimeRep). Type -> TYPE r -> Type where
--   MutableVector :: forall (r :: RuntimeRep) (s :: Type) (a :: TYPE r). MutableVector# @r s a -> MutableVector @r s a
-- 
-- type family Representation (r :: RuntimeRep) :: TYPE r -> TYPE 'UnliftedRep where
--   Representation 'LiftedRep = SmallArray#
--   Representation 'IntRep = ConstByteArray#
--   Representation 'WordRep = ConstByteArray#
--   Representation 'Word8Rep = ConstByteArray#
-- 
-- type family MutableRepresentation (r :: RuntimeRep) :: Type -> TYPE r -> TYPE 'UnliftedRep where
--   MutableRepresentation 'LiftedRep = SmallMutableArray#
--   MutableRepresentation 'IntRep = ConstMutableByteArray#
--   MutableRepresentation 'WordRep = ConstMutableByteArray#
--   MutableRepresentation 'Word8Rep = ConstMutableByteArray#
