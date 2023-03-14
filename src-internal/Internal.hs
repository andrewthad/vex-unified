{-# language TypeInType #-}
{-# language GADTs #-}
{-# language TypeFamilies #-}
{-# language RankNTypes #-}
{-# language TypeApplications #-}
{-# language MagicHash #-}
{-# language UnliftedNewtypes #-}

module Internal
  ( Length(..)
  ) where

import GHC.Exts
import Data.Kind (Type)

import GHC.TypeNats (Nat)

-- | Only intended to be used promoted.
data Length
  = Known Nat
  | Unknown
