{-# LANGUAGE BangPatterns #-}

module Data.Beamable.Int
    ( beamInt
    , unbeamInt
    , pokeWord8
    ) where

import Data.Beamable.Splits
import Blaze.ByteString.Builder
import qualified Blaze.ByteString.Builder.Internal.Write as Write
import Data.Bits ((.|.), (.&.), shift, shiftR)
import Data.Int (Int64)
import Data.Monoid ((<>))
import Foreign.Storable
import GHC.Word
import qualified Data.ByteString as B

{-
Beamed int representation:

1. The integer is chunked up into 7-bit groups, little-endian.
    Each of these 7bit chunks are encoded as a single octet.

2. All the octets except the last one has its 8th bit set.

3. 7th bit of the last octet represents sign.

0      | 0 0000000
1      | 0 0000001
63     | 0 0111111
64     | 1 1000000  0 0000000
127    | 1 1111111  0 0000000
128    | 1 0000000  0 0000001
8191   | 1 1111111  0 0111111
8192   | 1 0000000  1 1000000  0 0000000
65535  | 1 1111111  1 1111111  0 0000011
-1     | 0 1111111
-64    | 0 1000000
-65    | 1 0111111  0 1111111
-127   | 1 0000001  0 1111111
-128   | 1 0000000  0 1111111
-129   | 1 1111111  0 1111110
-8191  | 1 0000001  0 1000000
-8192  | 1 0000000  0 1000000
-8193  | 1 1111111  1 0111111  0 1111111
-}

beamInt :: Int64 -> Builder
beamInt !n = fromWrite $ Write.boundedWrite 10 $ beamIntPoke n
{-# INLINE beamInt #-}

beamIntPoke :: Int64 -> Write.Poke
beamIntPoke n
    | isZeroOrMinus1 (n `shiftR` 6) = pokeWord8 firstSeptet
    | otherwise =
        pokeWord8 (firstSeptet .|. 0x80) <> beamIntPoke next
    where
        firstSeptet :: Word8
        firstSeptet = fromIntegral $ n .&. 0x7F
        next = n `shiftR` 7

        isZeroOrMinus1 x = (x + 1) `shiftR` 1 == 0

pokeWord8 :: Word8 -> Write.Poke
pokeWord8 w = Write.pokeN 1 $ flip poke w

{-# INLINE unbeamInt #-}
-- This might not work well for 32bit platform
unbeamInt :: B.ByteString -> (Int64, B.ByteString)
unbeamInt bs = (fixSign (B.foldr' f 0 this), rest)
    where
        f :: Word8 -> Int64 -> Int64
        f w i = (i `shift` 7) .|. fromIntegral (w .&. 0x7F)

        fixSign :: Int64 -> Int64
        fixSign x = x `shift` (64 - l * 7) `shift` (l * 7 - 64)

        !l = B.length this
        !(!this, !rest) = splitAtLastWord bs
        -- }}}
