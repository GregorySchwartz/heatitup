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
    , spacerFalsePositive
    , consecutiveSpacerFalsePositive
    ) where

-- Standard
import Data.Char
import Data.List

-- Cabal
import qualified Data.ByteString.Char8 as C
import Text.EditDistance

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
itdFalsePositive :: Bool -> Distance -> ITD -> Bool
itdFalsePositive rev (Distance d) itd = check . bad $ rev
  where
    bad True  = "AATTTAG"
    bad False = "CTAAATT"
    check s = (<= d)
            . levenshteinDistance defaultEditCosts s
            . maybe s (C.unpack . unSubstring . _dupSubstring)
            . _duplication
            $ itd

-- | Check if the spacer is a false positive based on the Levenshtein
-- distance (ignoring the insertions and deletions). The threshold here is
-- a percentage of the spacer with a portion of the duplication (how much
-- of that string differs).
spacerFalsePositive :: Percent -> C.ByteString -> Substring -> Bool
spacerFalsePositive (Percent p) base (Substring s) =
    (<= (p / 100))
        . ( \x -> (fromIntegral x - expectedDistance)
          / (fromIntegral $ C.length s)
          )
        . levenshteinDistance defaultEditCosts (C.unpack base)
        . C.unpack
        $ s
  where
    expectedDistance = fromIntegral $ C.length base - C.length s

-- | Check if the spacer is a false positive based on the Levenshtein
-- distance (ignoring the insertions and deletions). The threshold here is
-- a percentage of the spacer with a portion of the duplication (how much
-- of that string differs).
consecutiveSpacerFalsePositive :: Consecutive
                               -> C.ByteString
                               -> Substring
                               -> Bool
consecutiveSpacerFalsePositive (Consecutive c) base (Substring s) =
    any (not . isConsecutive c . hammingList s . unSubstring)
        . fragmentSequence (Window . C.length $ s)
        $ base

-- | See if any x elements are false and neighboring in a list
isConsecutive :: Int -> [Bool] -> Bool
isConsecutive c = any (\x -> ((>= c) $ length x) && (not $ head x)) . group
