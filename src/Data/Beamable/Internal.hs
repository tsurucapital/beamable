{-# OPTIONS -Wall #-}
{-# LANGUAGE DeriveGeneric, TypeOperators, FlexibleContexts, DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, BangPatterns #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Beamable.Internal
    ( Beamable
    , beam
    , unbeam
    , typeSignR
    , typeSign
    , TypeSign (..)
    , Phantom (..)
    ) where

import Data.Beamable.Int
import Data.Beamable.Integer
import Data.Beamable.Splits
import Data.Beamable.Util

import Blaze.ByteString.Builder
import qualified Blaze.ByteString.Builder.Internal.Write as Write
import Data.Digest.Murmur64

import Control.Arrow (first)
import Data.Bits ((.|.), (.&.), shift, shiftL, shiftR, testBit)
import Data.ByteString (ByteString)
import Data.Char (ord, chr)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Monoid (mempty, mappend, mconcat, (<>))
import Data.Word (Word, Word8, Word16, Word32, Word64)
import Foreign.Storable
import GHC.Generics
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

class Beamable a where
    -- | Serialize value into 'Builder'
    beam :: a -> Builder
    -- | Deserialize next value from 'ByteString', also returns leftovers
    unbeam :: ByteString -> (a, ByteString)
    -- | Get value's type signature, should work fine on 'undefined' values
    -- takes list of strings with datatypes which already been traversed
    -- workaround to support recursive datatypes. In most cases you should be
    -- passing empty list in there.
    typeSignR :: [String] -> a -> Word64

    -- by default let's use generic version

    default beam :: (Generic a, GBeamable (Rep a)) => a -> Builder
    beam v = gbeam (from v) (0,0)

    default unbeam :: (Generic a, GBeamable (Rep a)) => ByteString -> (a, ByteString)
    unbeam v = first to $ gunbeam v (0,0)

    default typeSignR :: (Generic a, GBeamable (Rep a)) => [String] -> a -> Word64
    typeSignR prev v = gtypeSign prev (from v)

-- | Get type signature of arbitrary Beamable datatatype encoded as Word64 hash
-- with all constructors and datatypes in it. It's preferable to use 'typeSign'
-- against typeSignR, because implementation of later might change.
typeSign :: Beamable a => a -> Word64
typeSign = typeSignR []

signMur :: Hashable64 a => a -> Word64
signMur !a = asWord64 $ hash64 a

-- | It's possible to beam arbitrary Storable instances (not very size efficient)
beamStorable :: Storable a => a -> Builder-- {{{
beamStorable = fromStorable

unbeamStorable :: Storable a => ByteString -> (a, ByteString)
unbeamStorable bs = let v = peekBS bs in (v, B.drop (sizeOf v) bs)-- }}}

-- | It's possible to beam arbitrary Enum instances
beamEnum :: Enum a => a -> Builder-- {{{
beamEnum = beamInt . fromIntegral . fromEnum

unbeamEnum :: Enum a => ByteString -> (a, ByteString)
unbeamEnum bs = let (i, bs') = unbeamInt bs in (toEnum (fromIntegral i), bs')-- }}}


class GBeamable f where
    gbeam   :: f a        -> (Int, Word) -> Builder
    gunbeam :: B.ByteString -> (Int, Word) -> (f a, B.ByteString)
    gtypeSign :: [String] -> f a -> Word64

-- this instance used for datatypes with single constructor only
instance (GBeamable a, Datatype d, Constructor c) => GBeamable (M1 D d (M1 C c a)) where
    gbeam  (M1 (M1 x)) = gbeam x
    gunbeam x = first M1 . gunbeam x
    gtypeSign prev x | elem (datatypeName x) prev = signMur (datatypeName x, '_')
    gtypeSign prev x = signMur (datatypeName x, ':', gtypeSign (datatypeName x : prev) (unM1 x))

-- this instance used for  datatypes with multiple constructors and
-- values are prefixed by uniq number for each constructor
instance (GBeamable a, Constructor c) => GBeamable (M1 C c a) where
    gbeam (M1 x) t@(_, dirs) = mappend (beamWord $ fromIntegral dirs) (gbeam x t)
    {-# INLINE gbeam #-}
    gunbeam bs = first M1 . gunbeam bs
    gtypeSign prev x = signMur (conName x, '<', gtypeSign prev (unM1 x))

-- this instance is needed to avoid overlapping instances with (M1 D d (M1 C c a))
instance (Datatype d, GBeamable a, GBeamable b) => GBeamable (M1 D d (a :+: b) ) where
    gbeam (M1 x) = gbeam x
    gunbeam bs (lev, _) = let (dirs, bs') = unbeamWord bs
                            in first M1 $ gunbeam bs' (lev, fromIntegral dirs)
    gtypeSign prev x | elem (datatypeName x) prev  = signMur (datatypeName x, '_')

    gtypeSign prev x = signMur ( gtypeSign (datatypeName x : prev) (unL . unM1 $ x), '|'
                               , gtypeSign (datatypeName x : prev) (unR . unM1 $ x))

-- choose correct constructor based on the first word uncoded from the BS (dirs variable)
instance (GBeamable a, GBeamable b) => GBeamable (a :+: b) where
    gbeam (L1 x) (lev, dirs) = gbeam x (lev + 1, dirs)
    gbeam (R1 x) (lev, dirs) = gbeam x (lev + 1, dirs + (1 `shiftL` lev))
    gunbeam bs (lev, dirs) = if testBit dirs lev
                                   then first R1 $ gunbeam bs (lev + 1, dirs)
                                   else first L1 $ gunbeam bs (lev + 1, dirs)
    gtypeSign prev x = signMur (gtypeSign prev (unL x), '|', gtypeSign prev (unR x))

instance GBeamable a => GBeamable (M1 S c a) where
    gbeam (M1 x) = gbeam x
    gunbeam bs = first M1 . gunbeam bs
    gtypeSign prev ~(M1 x) = signMur ('[', gtypeSign prev x)

instance GBeamable U1 where
    gbeam _ _ = mempty
    gunbeam bs _ = (U1, bs)
    gtypeSign _ _x = signMur 'U'

instance (GBeamable a, GBeamable b) => GBeamable (a :*: b) where
    gbeam (x :*: y) t = gbeam x t `mappend` gbeam y t
    gunbeam bs t = let (ra, bs')  = gunbeam bs t
                       (rb, bs'') = gunbeam bs' t
                   in (ra :*: rb, bs'')
    gtypeSign prev ~(x :*: y) = signMur (gtypeSign prev x, '*', gtypeSign prev y)

instance Beamable a => GBeamable (K1 i a) where
    gbeam (K1 x) _ = beam x
    gunbeam bs   _ = first K1 (unbeam bs)
    gtypeSign prev x = signMur ('K', typeSignR prev (unK1 x))

{-
Beamed word representation:
    Same as positive ints, except that the last octet is dropped if it's 0.

63     | 0 0111111
64     | 1 1000000
127    | 1 1111111
128    | 1 0000000  0 0000001
-}

beamWord :: Word64 -> Builder
beamWord !n = fromWrite $ Write.boundedWrite 10 $ beamWordPoke n
{-# INLINE beamWord #-}

beamWordPoke :: Word64 -> Write.Poke
beamWordPoke n
    | next == 0 = pokeWord8 firstSeptet
    | otherwise = pokeWord8 (firstSeptet .|. 0x80) <> beamWordPoke next
    where
        firstSeptet :: Word8
        firstSeptet = fromIntegral $ n .&. 0x7F
        next = n `shiftR` 7

{-# INLINE unbeamWord #-}
unbeamWord :: B.ByteString -> (Word64, B.ByteString)
unbeamWord bs = (B.foldr' f 0 this, rest)
    where
        f :: Word8 -> Word64 -> Word64
        f w i = (i `shift` 7) .|. fromIntegral (w .&. 0x7F)
        !(!this, !rest) = splitAtLastWord bs
--}}

{-# SPECIALIZE beamWordX :: Word8 -> Builder #-}
{-# SPECIALIZE beamWordX :: Word16 -> Builder #-}
{-# SPECIALIZE beamWordX :: Word32 -> Builder #-}
{-# SPECIALIZE beamWordX :: Word64 -> Builder #-}
beamWordX :: Integral w => w -> Builder
beamWordX = beamWord . fromIntegral

{-# SPECIALIZE unbeamWordX :: B.ByteString -> (Word8, B.ByteString) #-}
{-# SPECIALIZE unbeamWordX :: B.ByteString -> (Word16, B.ByteString) #-}
{-# SPECIALIZE unbeamWordX :: B.ByteString -> (Word32, B.ByteString) #-}
{-# SPECIALIZE unbeamWordX :: B.ByteString -> (Word64, B.ByteString) #-}
unbeamWordX :: Integral w => B.ByteString -> (w, B.ByteString)
unbeamWordX bs = let (i, bs') = unbeamWord bs in (fromIntegral i, bs')-- }}}

newtype TypeSign = TypeSign { unTypeSign :: Word64 } deriving (Num, Show, Eq, Storable)

-- (de)serialization for numbers -- {{{
instance Beamable Int    where { beam = beamEnum ; unbeam = unbeamEnum ; typeSignR _ _ = signMur "Int" }
instance Beamable Int8   where { beam = beamEnum ; unbeam = unbeamEnum ; typeSignR _ _ = signMur "Int8" }
instance Beamable Int16  where { beam = beamEnum ; unbeam = unbeamEnum ; typeSignR _ _ = signMur "Int16" }
instance Beamable Int32  where { beam = beamEnum ; unbeam = unbeamEnum ; typeSignR _ _ = signMur "Int32" }
instance Beamable Int64  where { beam = beamInt ; unbeam = unbeamInt ; typeSignR _ _ = signMur "Int64" }
instance Beamable Word   where { beam = beamWordX ; unbeam = unbeamWordX ; typeSignR _ _ = signMur "Word" }
instance Beamable Word8  where { beam = beamWordX ; unbeam = unbeamWordX ; typeSignR _ _ = signMur "Word8" }
instance Beamable Word16 where { beam = beamWordX ; unbeam = unbeamWordX ; typeSignR _ _ = signMur "Word16" }
instance Beamable Word32 where { beam = beamWordX ; unbeam = unbeamWordX ; typeSignR _ _ = signMur "Word32" }
instance Beamable Word64 where { beam = beamWord ; unbeam = unbeamWord ; typeSignR _ _ = signMur "Word64" }
instance Beamable Float  where { beam = beamStorable ; unbeam = unbeamStorable ; typeSignR _ _ = signMur "Float" }
instance Beamable Double where { beam = beamStorable ; unbeam = unbeamStorable ; typeSignR _ _ = signMur "Double" }
instance Beamable TypeSign where { beam = beamStorable ; unbeam = unbeamStorable ; typeSignR _ _ = signMur "TypeSign" }
instance Beamable Integer where { beam = beamInteger ; unbeam = unbeamInteger ; typeSignR _ _ = signMur "Integer" }
-- }}}

instance Beamable Char where
    beam = beamWord . fromIntegral . ord
    unbeam = first (chr . fromIntegral) . unbeamWord
    typeSignR _ _ = signMur "Char"

-- Unit
instance Beamable () where
    beam          = const mempty
    unbeam bs     = ((), bs)
    typeSignR _ _ = signMur "()"

-- Tuples
instance (Beamable a, Beamable b) => Beamable (a, b)
instance (Beamable a, Beamable b, Beamable c) => Beamable (a, b, c)
instance (Beamable a, Beamable b, Beamable c, Beamable d) => Beamable (a, b, c, d)
instance (Beamable a, Beamable b, Beamable c, Beamable d
         ,Beamable e) => Beamable (a, b, c, d, e)
instance (Beamable a, Beamable b, Beamable c, Beamable d
         ,Beamable e, Beamable f) => Beamable (a, b, c, d, e, f)
instance (Beamable a, Beamable b, Beamable c, Beamable d
         ,Beamable e, Beamable f, Beamable g) => Beamable (a, b, c, d, e, f, g)

instance (Beamable a, Beamable b) => Beamable (Either a b)
instance Beamable a => Beamable (Maybe a)

instance Beamable Bool

instance Beamable a => Beamable [a] where
    {-# INLINE beam #-}
    beam xs = mconcat (beamWord (fromIntegral $ length xs):(map beam xs) )
    {-# INLINE unbeam #-}
    unbeam bs = let !(!cnt, bs') = unbeamWord bs
                  in unfoldCnt (fromIntegral cnt) unbeam bs'
    typeSignR prev _ = signMur ('L', typeSignR prev (undefined :: a))

{-# INLINE unfoldCnt #-}
unfoldCnt :: Int -> (b -> (a, b)) -> b -> ([a], b)
unfoldCnt cnt_i f = unfoldCnt' id cnt_i
    where
        unfoldCnt' xs 0 b = (xs [], b)
        unfoldCnt' xs cnt b = let !(!x, b') = f b
                              in unfoldCnt' (xs.(x:)) (cnt - 1) b'

instance Beamable ByteString where
    beam bs = beamWord (fromIntegral $ B.length bs) `mappend` fromByteString bs
    unbeam = uncurry (B.splitAt . fromIntegral) . unbeamWord
    typeSignR _ _ = signMur "ByteString.Strict"

instance Beamable BL.ByteString where
    beam = beam . BL.toChunks
    unbeam bs = let (chunks, bs') = unbeam bs
                  in (BL.fromChunks chunks, bs')
    typeSignR _ _ = signMur "ByteString.Lazy"

-- | @Phantom a@ has just one possible value, like @()@, and is encoded
-- as a 0-byte sequence. However, its 'typeSign' depends on the 'typeSign'
-- of its parameter.
data Phantom a = Phantom
    deriving (Show, Eq, Ord, Enum)

instance Beamable a => Beamable (Phantom a) where
    beam = const mempty
    unbeam bs = (Phantom, bs)
    typeSignR f _ =
        let thisDT = "Data.Beamable.Phantom"
            f' = thisDT:f
        in if elem thisDT f
               then signMur (thisDT, '_')
               else signMur (thisDT, ':', typeSignR f' (undefined :: a))
