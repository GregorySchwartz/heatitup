{- Utility
Gregory W. Schwartz

Collections the functions of general use in the program.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Utility
    ( zipMaybe
    , fragmentSequence
    , fragmentSequenceFiller
    , hammingList
    , hamming
    , isOverlappingBySubstring
    , itdFalsePositive
    , spacerFalsePositive
    , consecutiveSpacerFalsePositive
    , isConsecutive
    , getRichness
    , ignore
    ) where

-- Standard
import Data.Char
import Data.List
import Data.Maybe
import Data.Monoid
import qualified Data.Foldable as F
import qualified Data.Set as Set

-- Cabal
import qualified Data.ByteString.Char8 as C
import Data.Fasta.ByteString
import Text.EditDistance

-- Local
import Types


-- | Get the bs that correspond to the Justs in the first list.
zipMaybe :: [Maybe a] -> [b] -> [Maybe b]
zipMaybe xs = zipWith test xs
  where
    test (Just _) x = Just x
    test Nothing x  = Nothing

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

-- | Fragment the sequence by a certain window length, adding filler
fragmentSequenceFiller :: Window -> C.ByteString -> [Substring]
fragmentSequenceFiller (Window w) =
    go [] . flip C.append filler . C.append filler
  where
    filler = C.replicate (w - 1) '-'
    go !acc xs
        | C.null xs       = acc
        | C.length xs < w = acc
        | otherwise       = go (Substring (C.take w xs) : acc)
                          . C.drop 1
                          $ xs

-- | Get the hamming list between two strings, ignoring a character.
hammingList :: Maybe Char -> C.ByteString -> C.ByteString -> [Bool]
hammingList Nothing xs  = C.zipWith (==) xs
hammingList (Just i) xs = C.zipWith (\x y -> (x == y) || x == i || y == i) xs

-- | Get the hamming distance between two strings, ignoring a character.
hamming :: Maybe Char -> C.ByteString -> C.ByteString -> Int
hamming i xs = length . filter (not . id) . hammingList i xs

-- | Check if positions overlap
isOverlappingByPosition :: Window -> Position -> Position -> Bool
isOverlappingByPosition (Window w) (Position x) (Position y) = abs (x - y) < w

-- | Check if positions overlap at the substring level
isOverlappingBySubstring :: Duplication -> Bool
isOverlappingBySubstring (Duplication s [p1, p2] _) =
    isOverlappingByPosition (Window $ C.length . unSubstring $ s) p1 p2
isOverlappingBySubstring _ =
    error "Multiple locations found when checking for overlap"

-- | Check if a string is a false positive
itdFalsePositive :: Blacklist -> Distance -> String -> Bool
itdFalsePositive (Blacklist blacklist) (Distance d) dup =
    F.any ((<= d) . levenshteinDistance defaultEditCosts dup) blacklist

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
                               -> Maybe Char
                               -> C.ByteString
                               -> Substring
                               -> Bool
consecutiveSpacerFalsePositive (Consecutive c) ignore base (Substring s) =
    any (not . isConsecutive c . hammingList ignore s . unSubstring)
        . fragmentSequence (Window . C.length $ s)
        $ base

-- | See if any x elements are false and neighboring in a list
isConsecutive :: Int -> [Bool] -> Bool
isConsecutive c = any (\x -> ((>= c) $ length x) && (not $ head x)) . group

-- | Get the richness of a list.
getRichness :: (Eq a, Ord a) => [a] -> Int
getRichness = Set.size . Set.fromList

-- | Check to see if a fasta sequence should be analyzed.
ignore :: Int -> FastaSequence -> Maybe FastaSequence
ignore field fs = case getField field '|' fs of
                    "0" -> Nothing
                    "1" -> Just fs
                    _   -> error
                         $ "ignore-field process: neither a 0 nor 1 found at position "
                        <> show field
