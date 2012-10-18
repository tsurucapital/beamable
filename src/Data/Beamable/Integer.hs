{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module Data.Beamable.Integer
    ( beamInteger
    , unbeamInteger
    ) where

import Data.ByteString (ByteString)
import Data.Monoid (mappend)
import Blaze.ByteString.Builder
import qualified Data.ByteString as B
import GHC.Integer.GMP.Internals
import GHC.Base
import GHC.Word
import GHC.ST

import Data.Beamable.Int

beamInteger :: Integer -> Builder
beamInteger (S# x#)     = beamInt (-1) `mappend` beamInt (I# x#)
beamInteger (J# x# ba#) =
    beamInt (I# baSize#) `mappend` beamInt (I# x#) `mappend`
    fromWord8s [W8# (indexWord8Array# ba# i#) | I# i# <- [0 .. baSize - 1]]
  where
    baSize# = sizeofByteArray# ba#
    baSize  = I# baSize#
{-# INLINE beamInteger #-}

unbeamInteger :: ByteString -> (Integer, ByteString)
unbeamInteger bs
    | baSize# <# 0# = (S# x#, bs'')
    | otherwise  = runSTRep $ \s# ->
        let (# s'#, mba# #)  = newByteArray# baSize# s#
            s''#             = go mba# 0# s'#
            (# s'''#, ba# #) = unsafeFreezeByteArray# mba# s''#

        in (# s'''#, (J# x# ba#, B.drop (I# baSize#) bs'') #)
  where
    !(I# baSize#, bs') = unbeamInt bs
    !(I# x#, bs'')     = unbeamInt bs'

    go mba# i# s#
        | i# >=# baSize# = s#
        | otherwise      =
            let !(W8# b#) = B.index bs'' (I# i#)
                s'#       = writeWord8Array# mba# i# b# s#
            in go mba# (i# +# 1#) s'#
{-# INLINE unbeamInteger #-}
