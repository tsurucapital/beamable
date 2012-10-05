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
    , beamIt
    , unbeamIt
    , typeSign
    , encode
    , decode
    , encodeSigned
    , decodeSigned

    -- reexport
    , Builder
    , toByteString
    ) where

import Data.Beamable.Internal

import Blaze.ByteString.Builder

import qualified Data.ByteString as B





-- | Encode single value into compact bytestring, for encoding large number of values
-- use beamIt and toByteString from Blaze.ByteString.Builder
encode :: Beamable a => a -> B.ByteString
encode = toByteString . beamIt

-- | Decode single value from bytestring. ByteString must be exactly correct size
decode :: Beamable a => B.ByteString -> a
decode bs = case unbeamIt bs of
        (a, rest) | B.null rest -> a
        _ -> error $ "Beam decode failed: There are some leftovers!"

-- | Encode single value with extra type singature added, this adds 8 bytes to
-- binary representation, but will prevent decoding using invalid data instances
encodeSigned :: Beamable a => a -> B.ByteString
encodeSigned a = toByteString $ beamIt (typeSign a, a)

-- | Decode single value encoded with encodeSigned
decodeSigned :: Beamable a => B.ByteString -> a
decodeSigned bs = case unbeamIt bs of
        ((s, a), rest) | B.null rest && s == typeSign a -> a
        _ -> error $ "Beam decode failed: Type signature mismatch!"
