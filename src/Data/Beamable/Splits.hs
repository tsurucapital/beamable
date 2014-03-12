{-# LANGUAGE BangPatterns #-}

module Data.Beamable.Splits (
  splitAtLastWord,
) where

import Data.Bits ((.&.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B

{-# INLINE splitAtLastWord #-}
splitAtLastWord :: B.ByteString -> (B.ByteString, B.ByteString)
splitAtLastWord bs = let err = error "corrupted stream"
                         !pos = maybe err (+1) $ B.findIndex (\w -> w .&. 0x80 /= 0x80) bs
                         !this = B.unsafeTake pos bs
                         !rest = B.unsafeDrop pos bs
                     in (this, rest)


