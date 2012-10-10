{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Beamable.Tests
    ( tests
    ) where

import Blaze.ByteString.Builder
import Control.Applicative (pure, (<$>), (<*>))
import Control.Monad (replicateM)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import Data.Int
import Data.Word
import GHC.Generics (Generic)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

import Data.Beamable

tests :: Test
tests = testGroup "Data.Beamable.Tests"
    [ testProperty "testBeamable Words"
        (testBeamable :: TestBeamable (Word8, Word16, Word32, Word64, Word))
    , testProperty "testBeamable Int"
        (testBeamable :: TestBeamable Int)
    , testProperty "testBeamable Ints"
        (testBeamable :: TestBeamable (Int8, Int16, Int32, Int64, Int))
    , testProperty "testBeamable Either"
        (testBeamable :: TestBeamable (Either Double Float))
    , testProperty "testBeamable Bool"
        (testBeamable :: TestBeamable (Maybe Bool))
    , testProperty "testBeamable BS"
        (testBeamable :: TestBeamable B.ByteString)
    , testProperty "testBeamable LBS"
        (testBeamable :: TestBeamable BL.ByteString)
    , testProperty "testBeamable String"
        (testBeamable :: TestBeamable String)
    , testProperty "testBeamable Structure"
        (testBeamable :: TestBeamable Structure)
    ]

type TestBeamable a = a -> Bool

testBeamable :: (Arbitrary a, Beamable a, Eq a) => TestBeamable a
testBeamable value = B.null bs' && value' == value
  where
    bs            = toByteString $ beamIt value
    (value', bs') = unbeamIt bs

instance Arbitrary B.ByteString where
    arbitrary = C8.pack `fmap` arbitrary
    shrink = map C8.pack . shrink . C8.unpack

instance Arbitrary BL.ByteString where
    arbitrary = BL.pack `fmap` arbitrary
    shrink = map BL.pack . shrink . BL.unpack

-- | A large, somewhat arbitrary datastructures
data Structure
    = SEmpty
    | SList [Structure]
    | SString String
    | SAppend Structure Structure
    deriving (Eq, Generic, Show)

instance Beamable Structure

instance Arbitrary Structure where
    arbitrary = choose (1, 5) >>= arbitraryStructure

arbitraryStructure :: Int -> Gen Structure
arbitraryStructure s
    | s < 0     = return SEmpty
    | otherwise = oneof
        [ pure SEmpty
        , choose (0, 10) >>= \n ->
            SList <$> replicateM n (arbitraryStructure (s - 1))
        , SString <$> arbitrary
        , SAppend <$> arbitraryStructure (s - 1) <*> arbitraryStructure (s - 1)
        ]
