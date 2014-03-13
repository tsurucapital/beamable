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
    , Phantom(..)

    -- reexport
    , Builder
    , toByteString
    ) where

import Data.Beamable.Internal

import Blaze.ByteString.Builder

import qualified Data.ByteString as B
import Data.Word (Word)

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
encodeSigned a = toByteString $ beam (TypeSign (typeSign a), a)

-- | Decode single value encoded with encodeSigned
decodeSigned :: Beamable a => B.ByteString -> a
decodeSigned bs = case unbeam bs of
        ((s, a), rest) | B.null rest && s == TypeSign (typeSign a) -> a
        _ -> error $ "Beam decode failed: Type signature mismatch!"


encodeLive :: Beamable a => a -> B.ByteString
encodeLive a = toByteString . beam . toByteString . beam $ (TypeSign (typeSign a), a)

decodeLive :: Beamable a => Decoder a
decodeLive = WantAnyData

feed :: Beamable a => Decoder a -> B.ByteString -> Either (Decoder a) (a, B.ByteString)
feed state next = case state of
        WantAnyData | validPrefix next -> handlePrefix next
        WantAnyData -> Left (WantPrefix next)

        WantPrefix prev ->
            let bs = prev `B.append` next
            in if validPrefix bs
                   then handlePrefix bs
                   else Left (WantPrefix bs)

        WantBytes l prev ->
            let bs = prev `B.append` next
            in if B.length bs >= l
                   then doneDecoding bs
                   else Left (WantBytes l bs)
    where

        handlePrefix :: Beamable a => B.ByteString -> Either (Decoder a) (a, B.ByteString)
        handlePrefix bs | B.null bs = Left WantAnyData
        handlePrefix bs =
            let bsLength = (fromIntegral :: Word -> Int) $ fst $ unbeam bs
                Just prefixLength = succ `fmap` B.findIndex (<128) bs
                fullLength = bsLength + prefixLength
            in if fullLength <= B.length bs
                   then doneDecoding bs
                   else Left (WantBytes fullLength bs)

        -- valid prefix means that there is proper word64 encoded somewhere at the front =
        -- it's end marked by a byte with 8th bit off
        validPrefix :: B.ByteString -> Bool
        validPrefix = B.any (<128)

        doneDecoding :: Beamable a => B.ByteString -> Either (Decoder a) (a, B.ByteString)
        doneDecoding bs = let (bs', rest) = unbeam bs
                              ((s, a), _) = unbeam bs'
                          in if s == TypeSign (typeSign a)
                                then Right (a, rest)
                                else error $ "Beam decode failed: Type signature mismatch!"
