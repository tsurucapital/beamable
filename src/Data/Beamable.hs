{-# OPTIONS -Wall #-}

-- | To serialize your own datatype first you need to add DeriveGeneric pragma to the module
-- where your data is declared, derive Generic instance for that datatype and add empty
-- instance declaration for Beamable class
--
--
-- > {-# LANGUAGE DeriveGeneric #-}
--
-- > data Foo = Foo1 Int | Foo2 String deriving (Generic}
-- > instance Beamable Foo

module Data.Beamable
    ( Beamable
    , beam
    , unbeam
    , typeSign
    , encode
    , decode
    , encodeSigned
    , decodeSigned
    , Decoder
    , encodeLive
    , decodeLive
    , feed

    -- reexport
    , Builder
    , toByteString
    ) where

import Data.Beamable.Internal

import Blaze.ByteString.Builder

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import Data.Word (Word64)

data Decoder a
    = WantAnyData
    | WantPrefix B.ByteString
    | WantBytes Int B.ByteString



-- | Encode single value into compact bytestring, for encoding large number of values
-- use beam and toByteString from Blaze.ByteString.Builder
encode :: Beamable a => a -> B.ByteString
encode = toByteString . beam

-- | Decode single value from bytestring. ByteString must be exactly correct size
decode :: Beamable a => B.ByteString -> a
decode bs = case unbeam bs of
        (a, rest) | B.null rest -> a
        _ -> error $ "Beam decode failed: There are some leftovers!"

-- | Encode single value with extra type singature added, this adds 8 bytes to
-- binary representation, but will prevent decoding using invalid data instances
encodeSigned :: Beamable a => a -> B.ByteString
encodeSigned a = toByteString $ beam (typeSign a, a)

-- | Decode single value encoded with encodeSigned
decodeSigned :: Beamable a => B.ByteString -> a
decodeSigned bs = case unbeam bs of
        ((s, a), rest) | B.null rest && s == typeSign a -> a
        _ -> error $ "Beam decode failed: Type signature mismatch!"


encodeLive :: Beamable a => a -> B.ByteString
encodeLive a = toByteString $ beam (i2w $ BL.length encoded, encoded)
    where
        encoded :: BL.ByteString
        encoded = toLazyByteString $ beam (typeSign a, a)

        i2w :: Int64 -> Word64
        i2w = fromIntegral


decodeLive :: Beamable a => Decoder a
decodeLive = WantAnyData

feed :: Beamable a => Decoder a -> B.ByteString -> Either (Decoder a) (a, B.ByteString)
feed s next = case s of
        WantAnyData | validPrefix next -> handlePrefix next
        WantAnyData -> Left (WantPrefix next)

        WantPrefix prev ->
            let bs' = prev `B.append` next
            in if validPrefix bs'
                   then handlePrefix bs'
                   else Left (WantPrefix bs')

        WantBytes l prev ->
            let bs' = prev `B.append` next
            in if B.length bs' >= l
                   then doneDecoding bs'
                   else Left (WantBytes l bs')
    where

        handlePrefix :: Beamable a => B.ByteString -> Either (Decoder a) (a, B.ByteString)
        handlePrefix bs =
            let (l, bs') = unbeam bs
            in if l <= B.length bs'
                   then doneDecoding bs'
                   else Left (WantBytes l bs')

        -- valid prefix means that there is proper word64 encoded somewhere at the front =
        -- it's end marked by a byte with 8th bit off
        validPrefix :: B.ByteString -> Bool
        validPrefix = B.any (<128)

        doneDecoding :: Beamable a => B.ByteString -> Either (Decoder a) (a, B.ByteString)
        doneDecoding = Right . unbeam
