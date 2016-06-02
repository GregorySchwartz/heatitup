{- Utility
Gregory W. Schwartz

Collections the functions of general use in the program.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Utility
    ( fragmentSequence
    , hammingList
    , isOverlappingBySubstring
    , itdFalsePositive
    ) where

-- Standard
import Data.Char

-- Cabal
import qualified Data.ByteString.Char8 as C

-- Local
import Types

-- | Fragment the sequence by a certain window length
fragmentSequence :: Window -> C.ByteString -> [Substring]
fragmentSequence (Window w) = go []
  where
    go !acc xs
        | C.null xs       = acc
        | C.length xs < w = acc
        | otherwise       = go (Substring (C.take w xs) : acc)
                          . C.drop 1
                          $ xs

-- | Get the hamming list between two strings
hammingList :: C.ByteString -> C.ByteString -> [Bool]
hammingList xs = C.zipWith (==) xs

-- | Check if positions overlap
isOverlappingByPosition :: Window -> Position -> Position -> Bool
isOverlappingByPosition (Window w) (Position x) (Position y) = abs (x - y) < w

-- | Check if positions overlap at the substring level
isOverlappingBySubstring :: Duplication -> Bool
isOverlappingBySubstring (Duplication s [p1, p2] _) =
    isOverlappingByPosition (Window $ C.length . unSubstring $ s) p1 p2
isOverlappingBySubstring _ =
    error "Multiple locations found when checking for overlap"

-- | Check if the duplication is a false positive
itdFalsePositive :: Bool -> ITD -> Bool
itdFalsePositive rev itd = check . bad $ rev
  where
    bad True  = "AATTTAG"
    bad False = "CTAAATT"
    check s = (<= 1)
            . sum
            . fmap (\x -> if x then 0 else 1)
            . hammingList s
            . maybe s (unSubstring . _dupSubstring)
            . _duplication
            $ itd
