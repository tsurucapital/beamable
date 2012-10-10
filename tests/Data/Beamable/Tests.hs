{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Beamable.Tests
    ( tests
    ) where

import Blaze.ByteString.Builder
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import Data.Int
import Data.Word
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Property, forAll)
import Test.QuickCheck.Arbitrary

import Data.Beamable

tests :: Test
tests = testGroup "Data.Beamable.Tests"
    [ testProperty "beamableWordProp"   beamableWordProp
    , testProperty "beamableIntProp"    beamableIntProp
    , testProperty "beamableIntsProp"   beamableIntsProp
    , testProperty "beamableEitherProp" beamableEitherProp
    , testProperty "beamableBoolProp"   beamableBoolProp
    , testProperty "beamableBSProp"     beamableBSProp
    , testProperty "beamableBSLProp"    beamableBSLProp
    , testProperty "beamableStringProp" beamableStringProp
    ]

beamableWordProp :: Property
beamableWordProp =
    forAll arbitrary $ \value ->
    let bs = toByteString $ beamIt value
        (value', bs') = unbeamIt bs
        in B.null bs' && value' == (value :: (Word8, Word16, Word32, Word64, Word))

beamableIntProp :: Property
beamableIntProp =
    forAll arbitrary $ \value ->
    let bs = toByteString $ beamIt value
        (value', bs') = unbeamIt bs
        in B.null bs' && value' == (value :: Int)

beamableIntsProp :: Property
beamableIntsProp =
    forAll arbitrary $ \value ->
    let bs = toByteString $ beamIt value
        (value', bs') = unbeamIt bs
        in B.null bs' && value' == (value :: (Int8, Int16, Int32, Int64, Int))

beamableEitherProp :: Property
beamableEitherProp =
    forAll arbitrary $ \value ->
    let bs = toByteString $ beamIt value
        (value', bs') = unbeamIt bs
        in B.null bs' && value' == (value :: [Either Double Float])

beamableBoolProp :: Property
beamableBoolProp =
    forAll arbitrary $ \value ->
    let bs = toByteString $ beamIt value
        (value', bs') = unbeamIt bs
        in B.null bs' && value' == (value :: Maybe Bool)

beamableBSProp :: Property
beamableBSProp =
    forAll arbitrary $ \value ->
    let bs = toByteString $ beamIt value
        (value', bs') = unbeamIt bs
        in B.null bs' && value' == (value :: B.ByteString)

beamableBSLProp :: Property
beamableBSLProp =
    forAll arbitrary $ \value ->
    let bs = toByteString $ beamIt value
        (value', bs') = unbeamIt bs
        in B.null bs' && value' == (value :: BL.ByteString)

beamableStringProp :: Property
beamableStringProp =
    forAll arbitrary $ \value ->
    let bs = toByteString $ beamIt value
        (value', bs') = unbeamIt bs
        in B.null bs' && value' == (value :: String)

instance Arbitrary B.ByteString where
    arbitrary = C8.pack `fmap` arbitrary
    shrink = map C8.pack . shrink . C8.unpack

instance Arbitrary BL.ByteString where
    arbitrary = BL.pack `fmap` arbitrary
    shrink = map BL.pack . shrink . BL.unpack
