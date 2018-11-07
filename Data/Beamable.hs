{-# LANGUAGE ScopedTypeVariables #-}

module Data.Beamable
    ( Beamable(..)
    , typeSign
    , toBS, encoderToBS
    , fromBS
    , toBSSign
    , fromBSSign

    , word
    , word8
    , word16
    , word32
    , word64
    , bytestring

    , getWord
    , getWord8
    , getWord16
    , getWord32
    , getWord64
    , getBytestring

    ) where

import Data.Beamable.Internal
import Data.ByteString (ByteString)
import Data.Proxy



-- | To serialize your own datatype first you need to add DeriveGeneric pragma to the module
-- where your data is declared, derive Generic instance for that datatype and add empty
-- instance declaration for Beamable class
--
--
-- > {-# LANGUAGE DeriveGeneric #-}
--
-- > data Foo = Foo1 Int | Foo2 String deriving (Generic}
-- > instance Beamable Foo


-- | Serialize Beamable value into a ByteString adding a type signature
toBSSign :: forall a. Beamable a => a -> ByteString
toBSSign v = toBS (typeSign (Proxy :: Proxy a), v)


-- | Deserialize Beamable value from  a ByteString additionally checking if signature is
-- matching
fromBSSign :: forall a. Beamable a => ByteString -> Maybe (a, ByteString)
fromBSSign bs = case fromBS bs of
    Just ((sgn, v), rest)
        | typeSign (Proxy :: Proxy a) == sgn -> Just (v, rest)
        | otherwise -> error "Beam: signed decode failed, type signature mismatch"
    Nothing -> Nothing

