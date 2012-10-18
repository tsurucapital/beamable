module Data.Beamable.Int
    ( beamInt
    , unbeamInt
    ) where

import Data.Bits ((.|.), (.&.), shift, testBit)
import qualified Data.ByteString as B
import Data.List (unfoldr)
import Data.Word (Word8)
import Data.Int (Int64)

import Blaze.ByteString.Builder

{-
Beamed int representation:

1. The integer is chunked up into 7-bit groups. Each of these 7bit
chunks are encoded as a single octet.

2. All the octets except the last one has its 8th bit set.

3. 7th bit of the first octet represents sign.

3. Octets with bits 1..7 containing only 1 or 0 can be ignored when it's not affecting the sign:

0      | 0 0000000
1      | 0 0000001
63     | 0 0111111
64     | 1 0000000  0 1000000
127    | 1 0000000  0 1111111
128    | 1 0000001  0 0000000
8191   | 1 0111111  0 1111111
8192   | 1 0000000  1 1000000  0 0000000
65535  | 1 0000011  1 1111111  0 1111111
-1     | 0 1111111
-64    | 0 1000000
-65    | 1 1111111  0 0111111
-127   | 1 1111111  0 0000001
-128   | 1 1111111  0 0000000
-129   | 1 1111110  0 0111111
-8191  | 1 1000000  0 0000001
-8192  | 1 1000000  0 0000000
-8193  | 1 1111111  1 0111111  0 1111111
-}

-- This might not work well for 32bit platform
beamInt :: Int64 -> Builder -- {{{
beamInt 0 = fromWord8 0
beamInt n = toBldr . bitmark . reverse . unfoldr f $ n
    where
        f :: Int64 -> Maybe (Word8, Int64)
        f 0 = Nothing
        f x = let w = fromIntegral x .&. 0x7F :: Word8
                  rest = x `shift` (negate 7)
              in Just (w, if rest == (-1) then 0 else rest)

        bitmark :: [Word8] -> [Word8]
        bitmark (w:[]) = [w]
        bitmark (w:ws) = (w .|. 0x80) : bitmark ws
        bitmark [] = []

        toBldr :: [Word8] -> Builder
        toBldr ws =
            let ws' = if testBit (head ws) 6
                        then if n > 0 then 0x80:ws else ws
                        else if n > 0 then ws else 0xFF:ws
            in fromWriteList writeWord8 ws'

-- This might not work well for 32bit platform
unbeamInt :: B.ByteString -> (Int64, B.ByteString)
unbeamInt bs = (fixSign (B.foldl f 0 this), rest)
    where
        f :: Int64 -> Word8 -> Int64
        f i w = (i `shift` 7) .|. fromIntegral (w .&. 0x7F)

        fixSign :: Int64 -> Int64
        fixSign x = x `shift` (64 - l * 7) `shift` (l * 7 - 64)

        Just lastWord = B.findIndex (not . flip testBit 7) bs
        l = lastWord + 1
        (this, rest) = B.splitAt l bs-- }}}
