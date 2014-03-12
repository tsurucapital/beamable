{-# LANGUAGE BangPatterns #-}
module Main where

import Criterion
import Criterion.Main
import Data.Beamable
import Data.List (sort)
import Data.Word

main :: IO ()
main = defaultMain
    [ bgroup "roundTrip" rtGroup
    ]
  where
    !_unused  = sum testWords
    !_unused' = sum testInts

rtGroup =
    [ bgroup "Int"  $ map mkBenchRT testInts
    , bgroup "Word" $ map mkBenchRT testWords
    ]

mkBenchRT x = bench (show x) $ whnf ((`asTypeOf` x) . decode . encode) x

test10s :: [Word]
test10s = 0:[10^x | x <- [0..10]]
testWords = sort $ test10s ++ map (*5) test10s

testInts :: [Int]
testInts = let xs = map fromIntegral testWords
           in reverse (map negate xs) ++ tail xs 
