{- Spacer
Gregory W. Schwartz

Collections the functions pertaining to finding the spacer for FLT3
internal tandem duplications.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

module Spacer
    ( getSpacer
    ) where

-- Standard
import Data.Maybe
import qualified Data.Set as Set

-- Cabal
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Search
import Data.List.CommonSubstring
import Safe
import Control.Lens

-- Local
import Types

getSpacer :: Bool -> MinSize -> Duplication -> Query -> Maybe Spacer
getSpacer revCompl
          minAtypicalSize
          (Duplication { _dupSubstring = s, _dupLocations = [p1, p2] })
          q
    | C.null . unSubstring $ spacer = Nothing
    | otherwise                     =
        Just $
            Spacer { _spacerSubstring      = spacer
                   , _spacerLocation       = spacerPos
                   , _spacerOtherLocations = otherSpacerPositions
                                             minAtypicalSize
                                             spacerPos
                                             p2
                                             (C.pack newFLT3)
                                             s
                                             spacer
                   }
  where
    spacerPos = Position $ unPosition p1 + dupLen
    spacer    = inbetweenSubstring (Length dupLen) p1 p2 q
    dupLen    = C.length . unSubstring $ s
    newFLT3   = whichFLT3 revCompl
    whichFLT3 True  = flt3Exon14RevCompl
    whichFLT3 False = flt3Exon14

-- | Change a string to be identical with the one duplication position
mutateQuery :: Maybe Position -> Substring -> String -> String
mutateQuery Nothing _ _ = []
mutateQuery (Just (Position p)) (Substring s) q =
    concat [beginning, C.unpack s, end]
  where
    beginning = take p q
    end       = drop (p + C.length s) $ q

-- | Get the positions that are different between the spacer and the FLT3
-- sequence
otherSpacerPositions :: MinSize
                     -> Position
                     -> Position
                     -> C.ByteString
                     -> Substring
                     -> Substring
                     -> [Position]
otherSpacerPositions (MinSize minAtypicalSize)
                     (Position spacerPos)
                     (Position p2)
                     base
                     (Substring s)
                     (Substring spacer) =
        fmap Position
            . Set.toList
            . Set.difference (Set.fromList spacerPoss)
            . Set.intersection (Set.fromList spacerPoss)
            . Set.fromList
            . commonPos
            $ commonStart
  where
    commonPos Nothing       = []
    commonPos (Just (Position x, Length l)) = [x .. x + l - 1]
    commonStart        = headMay
                       . catMaybes
                       . fmap ( fmap (over _1 (+ (Position spacerPos)))
                              . getNonOverlapIdx
                                (Substring base)
                                (Substring spacer)
                                (Substring s)
                              . Length
                              )
                       $ [0 .. minLen]
    spacerRight        = C.append spacer s
    minLen             = if C.length spacer <= minAtypicalSize
                             then 0
                             else C.length spacer - minAtypicalSize
    spacerPoss         = [spacerPos .. p2 - 1]

-- | Get the non overlapping index between two strings when taking
-- a certain amount of the second string
getNonOverlapIdx :: Substring
                 -> Substring
                 -> Substring
                 -> Length
                 -> Maybe (Position, Length)
getNonOverlapIdx (Substring base) (Substring spacer) (Substring s) (Length l) =
    fmap (,Length . C.length $ newString)
        . fmap
          (const (Position . head . nonOverlappingIndices newString $ original))
        . headMay
        . nonOverlappingIndices newString
        $ base
  where
    newString = C.drop l original
    original  = C.append spacer . C.take (C.length s `div` 3) $ s

-- | Find the string inbetween two indices of the duplication
inbetweenSubstring :: Length -> Position -> Position -> Query -> Substring
inbetweenSubstring (Length n) (Position x) (Position y) =
    Substring . C.take (y - x - n) . C.drop (x + n) . unQuery

-- | The FLT3 Exon 14 sequence
flt3Exon14 :: String
flt3Exon14 = "CAAACTCTAAATTTTCTCTTGGAAACTCCCATTTGAGATCATATTCATATTCTCTGAAATCAACGTAGAAGTACTCATTATCTGAGGAGCCGGTCACCTGTACCATCTGTAGCTGGCTTTCATACCTAAATTG"

flt3Exon14RevCompl :: String
flt3Exon14RevCompl = "CAATTTAGGTATGAAAGCCAGCTACAGATGGTACAGGTGACCGGCTCCTCAGATAATGAGTACTTCTACGTTGATTTCAGAGAATATGAATATGATCTCAAATGGGAGTTTCCAAGAGAAAATTTAGAGTTTG"
