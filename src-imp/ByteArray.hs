{-# language MagicHash #-}
{-# language RankNTypes #-}
{-# language UnboxedTuples #-}
{-# language TypeInType #-}
{-# language DataKinds #-}

module disabled ByteArray
-- x   ( UAR
-- x   , UA
-- x   , MUA
-- x   , size#
-- x   , unsafeFreeze#
-- x   , uninitialized#
-- x   ) where
-- x 
-- x import GHC.Exts
-- x import Data.Kind
-- x 
-- x type UAR = 'BoxedRep 'Unlifted
-- x type UA = ByteArray#
-- x type MUA = MutableByteArray#
-- x 
-- x size# :: ByteArray# -> Int#
-- x size# = sizeofByteArray#
-- x 
-- x unsafeFreeze# :: MutableByteArray# s -> State# s -> (# State# s, ByteArray# #)
-- x unsafeFreeze# = unsafeFreezeByteArray#
-- x 
-- x uninitialized# :: Int# -> State# s -> (# State# s, MutableByteArray# s #)
-- x uninitialized# = newByteArray#
