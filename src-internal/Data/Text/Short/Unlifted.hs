{-# language GADTSyntax #-}
{-# language KindSignatures #-}
{-# language MagicHash #-}
{-# language UnliftedNewtypes #-}

module Data.Text.Short.Unlifted
  ( ShortText#(..)
  , lift
  , unlift
  ) where

import Data.Text.Short (ShortText)
import Data.ByteString.Short.Internal as TS
import GHC.Exts (ByteArray#,Levity(Unlifted),RuntimeRep(BoxedRep),TYPE)

import qualified Data.Text.Short as TS
import qualified Data.Text.Short.Unsafe as TS

newtype ShortText# :: TYPE ('BoxedRep 'Unlifted) where
  ShortText# :: ByteArray# -> ShortText#

lift :: ShortText# -> ShortText
lift (ShortText# x) = TS.fromShortByteStringUnsafe (SBS x)

unlift :: ShortText -> ShortText#
unlift t = case TS.toShortByteString t of
  SBS x -> ShortText# x
