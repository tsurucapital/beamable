{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import Criterion
import Criterion.Main
import Control.DeepSeq
import Data.Beamable
import Data.List (sort)
import Data.Word
import GHC.Generics

main :: IO ()
main = defaultMain
    [ bgroup "roundTrip" $ allBenches roundTrip
    , bgroup "encode" $ allBenches encodeF
    ]
  where
    !_unused  = sum testWords
    !_unused' = sum testInts

-- TODO: generalize this such that we can easily benchmark decoding
-- by itself.  At present we can only benchmark decoding by looking at
-- the round-trip times.
allBenches :: (forall a. (Beamable a) => a -> a) -> [Benchmark]
allBenches f =
    [ bgroup "Int"  $ map (mkBenchGroup f) testInts
    , bgroup "Word" $ map (mkBenchGroup f) testWords
    , bench "[Int]" $ nf f testInts
    , bench "[Int]1000" $ nf f longList
    , bench "(Word,Word)" $ nf f
            $ head $ zip testWords (reverse testWords)
    , bench "TestG1" $ nf f (TestG1 10 20 :: TestG [Int])
    , bench "TestG2" $ nf f (TestG2 [1..5] 20 :: TestG [Int])
    ]
  where
    longList = replicate 1000 (1::Int)
    !_unused = sum longList

mkBenchGroup f x = bench (show x) $ whnf f x

roundTrip x = decode (encode x) `asTypeOf` x

-- we use the slightly odd type because it fits into 'allBenches'
-- easily.  encode produces a strict ByteString, so we can just force
-- it to WHNF to benchmark it.
encodeF :: Beamable a => a -> a
encodeF x = encode x `seq` x

test10s :: [Word]
test10s = [10^x | x <- [0,2..10]]
testWords = 0: (sort $ test10s ++ map (*5) test10s)

testInts :: [Int]
testInts = let xs = map fromIntegral testWords
           in reverse (map negate xs) ++ tail xs 

data TestG a = TestG1 Int Int | TestG2 a Word
    deriving (Eq, Show, Generic)

instance Beamable a => Beamable (TestG a)

instance NFData a => NFData (TestG a) where
    rnf (TestG1 a b) = a `seq` b `seq` ()
    rnf (TestG2 a b) = a `deepseq` b `seq` ()
