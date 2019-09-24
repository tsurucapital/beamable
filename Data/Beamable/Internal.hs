{-# LANGUAGE MagicHash, ScopedTypeVariables, BangPatterns, DefaultSignatures #-}
{-# LANGUAGE TypeOperators, ViewPatterns, LambdaCase, RankNTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}


module Data.Beamable.Internal
    ( Beamable(..)
    , typeSign
    , toBS, encoderToBS
    , fromBS, fromDecoder

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

    , T(..), E(..), Decoder(..), DecoderAction(..)
    ) where





import Control.Monad (replicateM)
import Data.Bits (shiftL, shiftR, testBit, (.|.), (.&.), Bits(..))
import Data.ByteString (ByteString)
import Data.Char
import Data.Digest.Murmur64
import Data.List (foldl')
import Data.Proxy
import Data.Set (Set)
import Data.Typeable
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr
import Foreign.Storable (poke, peek)
import GHC.Exts (Word#)
import GHC.Fingerprint.Type
import GHC.Generics
import GHC.Int
import GHC.Word

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.Set as Set
import System.IO.Unsafe (unsafeDupablePerformIO)
import Unsafe.Coerce


-- | Every datatype before serialization is represented as a dlist-like structure
-- of simple tokens - fixed width values, (Word8..Word64), compactly represented
-- Word (smaller values are represented as shorter strings - number is stored
-- using 7 bits per byte, high end bytes with all zeros are ignored), and a ByteString
-- which is copied as is.
data T
    = TWord64 !Word64     !T
    | TWord32 !Word32     !T
    | TWord16 !Word16     !T
    | TWord8  !Word8      !T
    | TWord   !Word       !T
    | TBS     !ByteString !T
    | TNil
    deriving Show


-- | dlist-like structure of tokens, forms a Monoid
newtype E = E (T -> T)

instance Show E where show (E x) = show $ x TNil

instance Semigroup E where
    (<>) = mappend

instance Monoid E where
    {-# INLINE mempty #-}
    mempty = E (\x -> x)
    {-# INLINE mappend #-}
    E a `mappend` E b = E (a . b)


class Typeable a => Beamable a where
    serialize :: a -> E
    deserialize :: Decoder a

    -- | Add a type signature from current datatype (Proxy a) to the mix (Hash64 -> Hash64)
    -- Set Hash64 is used by generic instances to break the loop for recursive datatypes
    typeSignR :: Proxy a -> Set Hash64 -> Hash64 -> Hash64

    default serialize :: (Generic a, GBeamable (Rep a)) => a -> E
    {-# INLINE serialize #-}
    serialize v = gserialize (from v)

    default deserialize :: (Generic a, GBeamable (Rep a)) => Decoder a
    {-# INLINE deserialize #-}
    deserialize = to <$> gdeserialize 0

    default typeSignR :: (Typeable (Rep a), Generic a, GBeamable (Rep a)) => Proxy a -> Set Hash64 -> Hash64 -> Hash64
    {-# INLINEABLE typeSignR #-}
    typeSignR p s = let Fingerprint w1 w2 = typeRepFingerprint . typeRep $ (Proxy :: Proxy (Rep a))
                        hash = hash64AddWord64 w1 $ hash64 w2
                      in if Set.member hash s
                        then id else gtypeSignR (from <$> p) (Set.insert hash s) . hash64AddWord64 w1 . hash64AddWord64 w2

data Decoder a
    = Decoder
    { runDecoder :: forall r. (a -> DecoderAction r) -> DecoderAction r
    }

data DecoderAction a
    = ConsumeWord   !(Word# -> DecoderAction a)
    | ConsumeWord8  !(Word# -> DecoderAction a)
    | ConsumeWord16 !(Word# -> DecoderAction a)
    | ConsumeWord32 !(Word# -> DecoderAction a)
    | ConsumeWord64 !(Word# -> DecoderAction a)
    | ConsumeByteString !Word !(ByteString -> DecoderAction a)
    | Fail String
    | Done a

instance Functor Decoder where
    {-# INLINE fmap #-}
    fmap f = \d -> Decoder $ \k -> runDecoder d (k . f)

instance Applicative Decoder where
    {-# INLINE pure #-}
    pure = \x -> Decoder $ \k -> k x

    {-# INLINE (<*>) #-}
    (<*>) = \df dx -> Decoder $ \k ->
                        runDecoder df (\f -> runDecoder dx (\x -> k (f x)))

instance Monad Decoder where
    return = pure

    {-# INLINE (>>=) #-}
    (>>=) = \dm f -> Decoder $ \k -> runDecoder dm (\m -> runDecoder (f m) k)

    {-# INLINE (>>) #-}
    (>>) = \dm dn -> Decoder $ \k -> runDecoder dm (\_ -> runDecoder dn k)

instance MonadFail Decoder where
    fail msg = Decoder $ \_ -> Fail msg



-- | 'word8' .. 'word64' - put width item to a ByteString
{-# INLINE word8 #-}
word8 :: Word8 -> E
word8 = E . TWord8

{-# INLINE word16 #-}
word16 :: Word16 -> E
word16 = E . TWord16

{-# INLINE word32 #-}
word32 :: Word32 -> E
word32 = E . TWord32

{-# INLINE word64 #-}
word64 :: Word64 -> E
word64 = E . TWord64

-- | Put compactly represented Word to a ByteString (1..10 bytes),
-- if you know that your words are generally bigger than (2^7)^8
-- it's better to use Word64
{-# INLINE word #-}
word :: Word -> E
word = E . TWord

{-# INLINE bytestring #-}
bytestring :: ByteString -> E
bytestring bs = E (TWord (fromIntegral $ B.length bs) . TBS bs)

-- | get an item from Decoder monad. if 'serialize' first puts word and then word8
-- deserialize should also first use  'getWord' and then get 'getWord8'.
{-# INLINE getWord8 #-}
getWord8 :: Decoder Word8
getWord8 = Decoder (\k -> ConsumeWord8 (\w# -> k (W8# w#)))

{-# INLINE getWord16 #-}
getWord16 :: Decoder Word16
getWord16 = Decoder (\k -> ConsumeWord16 (\w# -> k (W16# w#)))

{-# INLINE getWord32 #-}
getWord32 :: Decoder Word32
getWord32 = Decoder (\k -> ConsumeWord32 (\w# -> k (W32# w#)))

{-# INLINE getWord64 #-}
getWord64 :: Decoder Word64
getWord64 = Decoder (\k -> ConsumeWord64 (\w# -> k (W64# w#)))

{-# INLINE getWord #-}
getWord :: Decoder Word
getWord = Decoder (\k -> ConsumeWord (\w# -> k (W# w#)))

{-# INLINE getBytestring #-}
getBytestring :: Decoder ByteString
getBytestring = do
    len <- getWord
    Decoder (ConsumeByteString len)

{-# INLINEABLE typeSign #-}
-- | Generates 64 bit wide type signature representation
typeSign :: Beamable a => Proxy a -> Word64
typeSign p = asWord64 $ typeSignR p Set.empty minBound

-- Boring instances
instance Beamable Word where -- {{{
    {-# INLINE serialize #-}
    serialize = word
    {-# INLINE deserialize #-}
    deserialize = getWord
    {-# INLINEABLE typeSignR #-}
    typeSignR _ _ = hash64Add "Word"

-- | Word8 .. Word64 stored as fixed-width values
instance Beamable Word8 where
    {-# INLINE serialize #-}
    serialize = word8
    {-# INLINE deserialize #-}
    deserialize = getWord8
    {-# INLINEABLE typeSignR #-}
    typeSignR _ _ = hash64Add "Word8"

instance Beamable Word16 where
    {-# INLINE serialize #-}
    serialize = word16
    {-# INLINE deserialize #-}
    deserialize = getWord16
    {-# INLINEABLE typeSignR #-}
    typeSignR _ _ = hash64Add "Word16"

instance Beamable Word32 where
    {-# INLINE serialize #-}
    serialize = word32
    {-# INLINE deserialize #-}
    deserialize = getWord32
    {-# INLINEABLE typeSignR #-}
    typeSignR _ _ = hash64Add "Word32"

instance Beamable Word64 where
    {-# INLINE serialize #-}
    serialize = word64
    {-# INLINE deserialize #-}
    deserialize = getWord64
    {-# INLINEABLE typeSignR #-}
    typeSignR _ _ = hash64Add "Word64"


-- | Ints are also represented in a compact way, to do so sign
-- bit is stored as first bit of the number, 'abs' is taken
-- so high unsed bits are zero and the remaining bits are shifted.
-- This way Word compression logic can be effectively reused.
instance Beamable Int where
    {-# INLINE serialize #-}
    serialize = serialize . (intToWord :: Int -> Word) where
        intToWord :: Int -> Word
        intToWord i = fromIntegral $ let i2 = i `shiftL` 1
                                         sgn = i `shiftR` 63
                                      in i2 `xor` sgn

    {-# INLINE deserialize #-}
    deserialize = (wordToInt :: Word -> Int) <$> getWord where
        wordToInt :: Word -> Int
        wordToInt w = fromIntegral $ let w2 = w `shiftR` 1
                                      in negate (w .&. 1) `xor` w2

    {-# INLINEABLE typeSignR #-}
    typeSignR _ _ = hash64Add "Int"

instance Beamable Int8 where
    {-# INLINE serialize #-}
    serialize = serialize . (fromIntegral :: Int8 -> Word8)
    {-# INLINE deserialize #-}
    deserialize = (fromIntegral :: Word8 -> Int8) <$> getWord8
    {-# INLINEABLE typeSignR #-}
    typeSignR _ _ = hash64Add "Int8"

instance Beamable Int16 where
    {-# INLINE serialize #-}
    serialize = serialize . (fromIntegral :: Int16 -> Word16)
    {-# INLINE deserialize #-}
    deserialize = (fromIntegral :: Word16 -> Int16) <$> getWord16
    {-# INLINEABLE typeSignR #-}
    typeSignR _ _ = hash64Add "Int16"

instance Beamable Int32 where
    {-# INLINE serialize #-}
    serialize = serialize . (fromIntegral :: Int32 -> Word32)
    {-# INLINE deserialize #-}
    deserialize = (fromIntegral :: Word32 -> Int32) <$> getWord32
    {-# INLINEABLE typeSignR #-}
    typeSignR _ _ = hash64Add "Int32"

instance Beamable Int64 where
    {-# INLINE serialize #-}
    serialize = serialize . (fromIntegral :: Int64 -> Word64)
    {-# INLINE deserialize #-}
    deserialize = (fromIntegral :: Word64 -> Int64) <$> getWord64
    {-# INLINEABLE typeSignR #-}
    typeSignR _ _ = hash64Add "Int64"

instance Beamable Double where
    {-# INLINE serialize #-}
    serialize = word64 . unsafeCoerce
    {-# INLINE deserialize #-}
    deserialize = unsafeCoerce <$> getWord64
    {-# INLINEABLE typeSignR #-}
    typeSignR _ _ = hash64Add "Double"

instance Beamable Float where
    {-# INLINE serialize #-}
    serialize = word32 . unsafeCoerce
    {-# INLINE deserialize #-}
    deserialize = unsafeCoerce <$> getWord32
    {-# INLINEABLE typeSignR #-}
    typeSignR _ _ = hash64Add "Float"

instance Beamable () where
    {-# INLINE serialize #-}
    serialize = mempty
    {-# INLINE deserialize #-}
    deserialize = return ()
    {-# INLINEABLE typeSignR #-}
    typeSignR _ _ = hash64Add "()"

instance forall a. Beamable a => Beamable (Proxy a) where
    {-# INLINE serialize #-}
    serialize = mempty
    {-# INLINE deserialize #-}
    deserialize = return Proxy
    {-# INLINEABLE typeSignR #-}
    typeSignR _ s = hash64Add "Proxy" . typeSignR (Proxy :: Proxy a) s

instance forall a. Beamable a => Beamable (Maybe a) where
    {-# INLINEABLE serialize #-}
    serialize = maybe (word8 0) (\v -> word8 1 <> serialize v)
    {-# INLINEABLE deserialize #-}
    deserialize = do present <- getWord8
                     if present == 1
                        then Just <$> deserialize
                        else return Nothing
    {-# INLINEABLE typeSignR #-}
    typeSignR _ s = typeSignR (Proxy :: Proxy a) s . hash64Add "Maybe"

instance forall a b. (Beamable a, Beamable b) => Beamable (Either a b) where
    {-# INLINEABLE serialize #-}
    serialize = either (\v -> word8 0 <> serialize v)
                       (\v -> word8 1 <> serialize v)
    {-# INLINEABLE deserialize #-}
    deserialize = do dir <- getWord8
                     if dir == 1
                        then Right <$> deserialize
                        else Left <$> deserialize
    {-# INLINEABLE typeSignR #-}
    typeSignR _ s = typeSignR (Proxy :: Proxy a) s . typeSignR (Proxy :: Proxy b) s . hash64Add "Either"

instance Beamable Bool where
    {-# INLINE serialize #-}
    serialize x = if x then word8 1 else word8 0
    {-# INLINE deserialize #-}
    deserialize = (== 1) <$> getWord8
    {-# INLINEABLE typeSignR #-}
    typeSignR _ _ = hash64Add "Bool"


instance forall a b.
        ( Beamable a
        , Beamable b
        ) => Beamable (a, b) where
    {-# INLINEABLE serialize #-}
    serialize (a, b) = serialize a <> serialize b
    {-# INLINEABLE deserialize #-}
    deserialize = (,) <$> deserialize <*> deserialize
    {-# INLINEABLE typeSignR #-}
    typeSignR _ s = typeSignR (Proxy :: Proxy a) s
                  . typeSignR (Proxy :: Proxy b) s
                  . hash64Add "(,)"

instance forall a b c.
        ( Beamable a
        , Beamable b
        , Beamable c
        ) => Beamable (a, b, c) where
    {-# INLINEABLE serialize #-}
    serialize (a, b, c) = serialize a <> serialize b <> serialize c
    {-# INLINEABLE deserialize #-}
    deserialize = (,,) <$> deserialize <*> deserialize <*> deserialize
    {-# INLINEABLE typeSignR #-}
    typeSignR _ s = typeSignR (Proxy :: Proxy a) s
                  . typeSignR (Proxy :: Proxy b) s
                  . typeSignR (Proxy :: Proxy c) s
                  . hash64Add "(,,)"

instance forall a b c d.
        ( Beamable a
        , Beamable b
        , Beamable c
        , Beamable d
        ) => Beamable (a, b, c, d) where
    {-# INLINEABLE serialize #-}
    serialize (a, b, c, d) = serialize a <> serialize b <> serialize c <> serialize d
    {-# INLINEABLE deserialize #-}
    deserialize = (,,,) <$> deserialize <*> deserialize <*> deserialize <*> deserialize
    {-# INLINEABLE typeSignR #-}
    typeSignR _ s = typeSignR (Proxy :: Proxy a) s
                  . typeSignR (Proxy :: Proxy b) s
                  . typeSignR (Proxy :: Proxy c) s
                  . typeSignR (Proxy :: Proxy d) s
                  . hash64Add "(,,,)"

instance forall a b c d e.
        ( Beamable a
        , Beamable b
        , Beamable c
        , Beamable d
        , Beamable e
        ) => Beamable (a, b, c, d, e) where
    {-# INLINEABLE serialize #-}
    serialize (a, b, c, d, e) = serialize a <> serialize b <> serialize c <> serialize d
                             <> serialize e
    {-# INLINEABLE deserialize #-}
    deserialize = (,,,,) <$> deserialize <*> deserialize <*> deserialize <*> deserialize
                         <*> deserialize
    {-# INLINEABLE typeSignR #-}
    typeSignR _ s = typeSignR (Proxy :: Proxy a) s
                  . typeSignR (Proxy :: Proxy b) s
                  . typeSignR (Proxy :: Proxy c) s
                  . typeSignR (Proxy :: Proxy d) s
                  . typeSignR (Proxy :: Proxy e) s
                  . hash64Add "(,,,,)"

instance forall a b c d e f.
        ( Beamable a
        , Beamable b
        , Beamable c
        , Beamable d
        , Beamable e
        , Beamable f
        ) => Beamable (a, b, c, d, e, f) where
    {-# INLINEABLE serialize #-}
    serialize (a, b, c, d, e, f) = serialize a <> serialize b <> serialize c <> serialize d
                                <> serialize e <> serialize f
    {-# INLINEABLE deserialize #-}
    deserialize = (,,,,,) <$> deserialize <*> deserialize <*> deserialize <*> deserialize
                          <*> deserialize <*> deserialize
    {-# INLINEABLE typeSignR #-}
    typeSignR _ s = typeSignR (Proxy :: Proxy a) s
                  . typeSignR (Proxy :: Proxy b) s
                  . typeSignR (Proxy :: Proxy c) s
                  . typeSignR (Proxy :: Proxy d) s
                  . typeSignR (Proxy :: Proxy e) s
                  . typeSignR (Proxy :: Proxy f) s
                  . hash64Add "(,,,,,)"

instance forall a b c d e f g.
        ( Beamable a
        , Beamable b
        , Beamable c
        , Beamable d
        , Beamable e
        , Beamable f
        , Beamable g
        ) => Beamable (a, b, c, d, e, f, g) where
    {-# INLINEABLE serialize #-}
    serialize (a, b, c, d, e, f, g) = serialize a <> serialize b <> serialize c <> serialize d
                                   <> serialize e <> serialize f <> serialize g
    {-# INLINEABLE deserialize #-}
    deserialize = (,,,,,,) <$> deserialize <*> deserialize <*> deserialize <*> deserialize
                           <*> deserialize <*> deserialize <*> deserialize
    {-# INLINEABLE typeSignR #-}
    typeSignR _ s = typeSignR (Proxy :: Proxy a) s
                  . typeSignR (Proxy :: Proxy b) s
                  . typeSignR (Proxy :: Proxy c) s
                  . typeSignR (Proxy :: Proxy d) s
                  . typeSignR (Proxy :: Proxy e) s
                  . typeSignR (Proxy :: Proxy f) s
                  . typeSignR (Proxy :: Proxy g) s
                  . hash64Add "(,,,,,,)"

instance forall a b c d e f g h.
        ( Beamable a
        , Beamable b
        , Beamable c
        , Beamable d
        , Beamable e
        , Beamable f
        , Beamable g
        , Beamable h
        ) => Beamable (a, b, c, d, e, f, g, h) where
    {-# INLINEABLE serialize #-}
    serialize (a, b, c, d, e, f, g, h)
        = serialize a <> serialize b <> serialize c <> serialize d
       <> serialize e <> serialize f <> serialize g <> serialize h
    {-# INLINEABLE deserialize #-}
    deserialize = (,,,,,,,) <$> deserialize <*> deserialize <*> deserialize <*> deserialize
                            <*> deserialize <*> deserialize <*> deserialize <*> deserialize
    {-# INLINEABLE typeSignR #-}
    typeSignR _ s = typeSignR (Proxy :: Proxy a) s
                  . typeSignR (Proxy :: Proxy b) s
                  . typeSignR (Proxy :: Proxy c) s
                  . typeSignR (Proxy :: Proxy d) s
                  . typeSignR (Proxy :: Proxy e) s
                  . typeSignR (Proxy :: Proxy f) s
                  . typeSignR (Proxy :: Proxy g) s
                  . typeSignR (Proxy :: Proxy h) s
                  . hash64Add "(,,,,,,,)"

instance Beamable a => Beamable [a] where
    {-# INLINEABLE serialize #-}
    serialize xs = let (acc, len) = foldl' (\(a, l) x -> (a <> serialize x, l + 1)) (mempty, 0) xs
                    in serialize (len :: Word) <> acc
    {-# INLINEABLE deserialize #-}
    deserialize = deserialize >>= \w -> replicateM (fromIntegral (w :: Word)) deserialize
    {-# INLINEABLE typeSignR #-}
    typeSignR p s = typeSignR (head <$> p) s . hash64Add "[]"


instance Beamable ByteString where
    {-# INLINE serialize #-}
    serialize = bytestring
    {-# INLINE deserialize #-}
    deserialize = getBytestring
    {-# INLINEABLE typeSignR #-}
    typeSignR _ _ = hash64Add "ByteString"

instance Beamable Char where
    {-# INLINE serialize #-}
    serialize = serialize . ord
    {-# INLINE deserialize #-}
    deserialize = chr <$> deserialize
    {-# INLINEABLE typeSignR #-}
    typeSignR _ _ = hash64Add "ByteString"
-- }}}


-- Generic instances
class GBeamable f where-- {{{
    gserialize :: f a -> E
    gdirs :: f a -> Int -> Word -> E
    {-# INLINE gdirs #-}
    gdirs _ _ _ = mempty
    gdeserialize :: Word -> Decoder (f a)
    gtypeSignR :: Proxy (f a) -> Set Hash64 -> Hash64 -> Hash64

instance (GBeamable a, Datatype d, Constructor c) => GBeamable (M1 D d (M1 C c a)) where
    {-# INLINE gserialize #-}
    gserialize (M1 (M1 x)) = gserialize x

    {-# INLINE gdeserialize #-}
    gdeserialize _ = (M1 . M1) <$> gdeserialize undefined
    {-# INLINE gtypeSignR #-}
    gtypeSignR p s = gtypeSignR (unM1 <$> p) s

instance (GBeamable a, Constructor c) => GBeamable (M1 C c a) where
    {-# INLINE gserialize #-}
    gdirs _ _ dirs = word dirs
    {-# INLINE gdirs #-}
    gserialize (M1 x) = gserialize x
    {-# INLINE gdeserialize #-}
    gdeserialize dirs =  M1 <$> (gdeserialize dirs)
    {-# INLINE gtypeSignR #-}
    gtypeSignR p s = let con = hash64 ('C':' ':conName (undefined `asProxyTypeOf` p))
                      in gtypeSignR (unM1 <$> p) (Set.insert con s) . hash64Add (asWord64 con)

instance (GBeamable a, GBeamable b) => GBeamable (M1 D d (a :+: b)) where
    {-# INLINE gserialize #-}
    gserialize (M1 x) = gdirs x 0 0 <> gserialize x
    {-# INLINE gdeserialize #-}
    gdeserialize _ =  M1 <$> (deserialize >>= gdeserialize)
    {-# INLINE gtypeSignR #-}
    gtypeSignR p s = gtypeSignR (unM1 <$> p) s

-- choose correct constructor based on the first word uncoded from the BS (dirs variable)
instance (GBeamable a, GBeamable b) => GBeamable (a :+: b) where
    {-# INLINE gdirs #-}
    gdirs (L1 x) lev dirs = gdirs x (lev + 1) dirs
    gdirs (R1 x) lev dirs = gdirs x (lev + 1) (dirs + 1 `shiftL` lev)
    {-# INLINE gserialize #-}
    gserialize (L1 x) = gserialize x
    gserialize (R1 x) = gserialize x
    {-# INLINE gdeserialize #-}
    gdeserialize dirs = if testBit dirs 0
                            then R1 <$> gdeserialize (dirs `shiftR` 1)
                            else L1 <$> gdeserialize (dirs `shiftR` 1)
    {-# INLINE gtypeSignR #-}
    gtypeSignR p s = gtypeSignR (unL <$> p) s . gtypeSignR (unR <$> p) s
        where
            unL :: (al :+: ar) aa -> al aa
            unL = undefined

            unR :: (al :+: ar) aa -> ar aa
            unR = undefined


instance GBeamable a => GBeamable (M1 S c a) where
    {-# INLINE gserialize #-}
    gserialize (M1 x) = gserialize x
    {-# INLINE gdeserialize #-}
    gdeserialize dirs = M1 <$> gdeserialize dirs
    {-# INLINE gtypeSignR #-}
    gtypeSignR p s = gtypeSignR (unM1 <$> p) s . hash64Add '['

instance GBeamable U1 where
    {-# INLINE gserialize #-}
    gserialize _ = mempty
    {-# INLINE gdeserialize #-}
    gdeserialize _ = return U1
    {-# INLINE gtypeSignR #-}
    gtypeSignR _ _ = hash64Add 'U'

instance (GBeamable a, GBeamable b) => GBeamable (a :*: b) where
    {-# INLINE gserialize #-}
    gserialize (x :*: y) = case gserialize x of
                                E xs -> case gserialize y of
                                    E ys -> E (\zs -> xs (ys zs))
    {-# INLINE gdeserialize #-}
    gdeserialize dirs = (:*:) <$> gdeserialize dirs <*> gdeserialize dirs
    {-# INLINE gtypeSignR #-}
    gtypeSignR p s = gtypeSignR (unLS <$> p) s . gtypeSignR (unRS <$> p) s
        where
            unLS :: (al :*: ar) aa -> al aa
            unLS = undefined

            unRS :: (al :*: ar) aa -> ar aa
            unRS = undefined

instance Beamable a => GBeamable (K1 i a) where
    {-# INLINE gserialize #-}
    gserialize (K1 x) = serialize x
    {-# INLINE gdeserialize #-}
    gdeserialize _ = K1 <$> deserialize
    {-# INLINE gtypeSignR #-}
    gtypeSignR p s = typeSignR (unK1 <$> p) s
    -- }}}

-- | pack 'Beamable a' into a 'ByteString', initially 'ByteString' of size ~4k is allocated
-- and it's size is doubled every time it's insufficient.
toBS :: Beamable a => a -> ByteString
toBS (serialize -> !x) = encoderToBS x

{-# INLINE encoderToBS #-}
encoderToBS :: E -> ByteString
encoderToBS (E !x0) = unsafeDupablePerformIO $ tryFitInto (4096 - 128)
    where
        tryFitInto bufSize = do
            fptr <- B.mallocByteString bufSize
            end <- withForeignPtr fptr $ \ptr -> do
                (flip minusPtr ptr) <$> go (ptr `plusPtr` bufSize) (x0 TNil) ptr
            if end < 0
                then tryFitInto (bufSize * 2)
                else return $ B.fromForeignPtr fptr 0 end

        packWord :: Word -> Ptr Word8 -> IO (Ptr Word8)
        packWord w ptr
                | w > 0x7F = do let !this = fromIntegral w .|. 0x80
                                    !next = w `shiftR` 7
                                poke ptr (this :: Word8)
                                packWord next (ptr `plusPtr` 1)
                | otherwise = do poke ptr ((fromIntegral w) :: Word8)
                                 return (ptr `plusPtr` 1)

        go lim this ptr =
          case this of
            TWord !x rest -> if ptr `plusPtr` 10 > lim
                            then return nullPtr
                            else packWord x ptr >>= go lim rest

            TWord8 !x rest -> let ptr' = ptr `plusPtr` 1
                         in if ptr' > lim
                            then return nullPtr
                            else poke (castPtr ptr) x >> go lim rest ptr'

            TWord16 !x rest -> let ptr' = ptr `plusPtr` 2
                         in if ptr' > lim
                            then return nullPtr
                            else poke (castPtr ptr) x >> go lim rest ptr'

            TWord32 !x rest -> let ptr' = ptr `plusPtr` 4
                         in if ptr' > lim
                            then return nullPtr
                            else poke (castPtr ptr) x >> go lim rest ptr'

            TWord64 !x rest -> let ptr' = ptr `plusPtr` 8
                         in if ptr' > lim
                            then return nullPtr
                            else poke (castPtr ptr) x >> go lim rest ptr'

            TBS (B.PS fptr f l) rest -> let ptr' = ptr `plusPtr` l
                         in if ptr' > lim
                            then return nullPtr
                            else do withForeignPtr fptr $ \bs ->
                                        copyBytes ptr (bs `plusPtr` f) l
                                    go lim rest ptr'

            TNil -> return $ castPtr ptr


-- | try to deserialize 'Beamable a' value from a 'ByteString', if
-- there's not enough input 'Nothing' will be produced, otherwise
-- a value plus unused 'ByteString'. By using leftovers it should
-- be possible to process an infinite stream of Beamable values
fromBS :: Beamable a => B.ByteString -> Maybe (a, B.ByteString) -- {{{
fromBS = fromDecoder deserialize
{-# INLINE fromBS #-}

fromDecoder :: forall a. Decoder a -> B.ByteString -> Maybe (a, B.ByteString)
fromDecoder _ bs | B.null bs = Nothing
fromDecoder decoder (B.PS fptr offs len) = unsafeDupablePerformIO $
        withForeignPtr fptr $ \ptr -> do
            let ptr' = ptr `plusPtr` offs
                lim' = ptr' `plusPtr` len
            go (runDecoder decoder Done) ptr' lim'
    where
        go :: DecoderAction a -> Ptr  Word8 -> Ptr Word8 -> IO (Maybe (a, B.ByteString))
        go act ptr lim = case act of
            ConsumeWord k -> do
                (W# w#, ptr') <- unpackWord 0 0 ptr lim
                if ptr' == nullPtr
                    then return Nothing
                    else go (k w#) ptr' lim

            ConsumeWord8 k -> let ptr' = ptr `plusPtr` 1
                in if ptr' > lim
                    then return Nothing
                    else peek ptr >>= \(W8# w#) -> go (k w#) ptr' lim

            ConsumeWord16 k -> let ptr' = ptr `plusPtr` 2
                in if ptr' > lim
                    then return Nothing
                    else peek (castPtr ptr) >>= \(W16# w#) -> go (k w#) ptr' lim

            ConsumeWord32 k -> let ptr' = ptr `plusPtr` 4
                in if ptr' > lim
                    then return Nothing
                    else peek (castPtr ptr) >>= \(W32# w#) -> go (k w#) ptr' lim

            ConsumeWord64 k -> let ptr' = ptr `plusPtr` 8
                in if ptr' > lim
                    then return Nothing
                    else peek (castPtr ptr) >>= \(W64# w#) -> go (k w#) ptr' lim

            ConsumeByteString (fromIntegral -> l) k -> let ptr' = ptr `plusPtr` l
                in if ptr' > lim
                    then return Nothing
                    else do offs' <- withForeignPtr fptr (return . minusPtr ptr)
                            let bs = B.copy (B.fromForeignPtr fptr offs' l)
                            go (k bs) ptr' lim

            Done a -> withForeignPtr fptr $ \ptr0 -> do
                let offs' = ptr `minusPtr` ptr0
                    len' = len - offs' + offs
                    bs' = if len' == 0
                            then B.empty
                            else B.fromForeignPtr fptr offs' len'
                return (Just (a, bs'))

            Fail _ -> return Nothing

        unpackWord :: Word -> Int -> Ptr Word8 -> Ptr Word8 -> IO (Word, Ptr Word8)
        unpackWord w0 s ptr lim = let ptr' = ptr `plusPtr` 1
            in if ptr' > lim
                then return (w0, nullPtr)
                else do
                    w8 <- peek ptr
                    let w = fromIntegral w8
                        s' = s + 7
                    if testBit w 7
                        then unpackWord (w0 .|. ((w .&. 0x7F) `shiftL` s)) s' ptr' lim
                        else return (w0 .|. (w `shiftL` s), ptr')
-- }}}
