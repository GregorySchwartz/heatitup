{- Utility
Gregory W. Schwartz

Collections the functions of general use in the program.
-}

{-# LANGUAGE BangPatterns #-}

module Utility
    ( fragmentSequence
    , hammingList
    , isOverlappingBySubstring
    ) where

-- Standard
import Data.Char

-- Cabal
import qualified Data.ByteString.Char8 as C
import Control.Lens

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

-- | Mutate a ByteString by the specified character and index
mutate :: Position -> Char -> C.ByteString -> C.ByteString
mutate (Position p) x = (&) (ix p .~ (fromIntegral . fromEnum $ x))
