{-# OPTIONS -Wall #-}
{-# LANGUAGE TypeOperators #-}

module Data.Beamable.Util
    ( peekBS
    , unL
    , unR
    ) where

import Data.ByteString.Internal (ByteString (..))
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import GHC.Generics
import System.IO.Unsafe (unsafeDupablePerformIO)

peekBS :: Storable a => ByteString -> a
peekBS (PS fptr offset _) = unsafeDupablePerformIO $ withForeignPtr fptr $ \ptr' ->
    let ptr = plusPtr ptr' offset
    in  peek (castPtr ptr)

unL :: (l :+: r) a -> l a
unL = error "unL should be used only for type recovery operations"

unR :: (l :+: r) a -> r a
unR = error "unR should be used only for type recovery operations"
