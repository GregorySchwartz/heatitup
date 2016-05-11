{- Spacer
Gregory W. Schwartz

Collections the functions pertaining to finding the spacer for FLT3
internal tandem duplications.
-}

{-# LANGUAGE BangPatterns #-}

module Spacer
    ( getSpacer
    ) where

-- Standard
import qualified Data.Set as Set
import Debug.Trace

-- Cabal
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Search
import Data.List.CommonSubstring

-- Local
import Types

getSpacer :: Bool -> Duplication -> Query -> Maybe Spacer
getSpacer revCompl
          (Duplication { _dupSubstring = s, _dupLocations = [p1, p2] })
          q
    | C.null . unSubstring $ spacer = Nothing
    | otherwise                     =
        Just $
            Spacer { _spacerSubstring      = spacer
                   , _spacerLocation       = pos
                   , _spacerOtherLocations = otherSpacerPositions
                                             pos
                                             p2
                                             common
                                             spacer
                   }
  where
    pos       = Position $ unPosition p1 + dupLen
    spacer    = inbetweenSubstring (Length dupLen) p1 p2 q
    dupLen    = C.length . unSubstring $ s
    common    = C.pack
              . longestSubstring (whichFLT3 revCompl)
              . C.unpack
              . unSubstring
              $ spacer
    whichFLT3 True  = flt3Exon14RevCompl
    whichFLT3 False = flt3Exon14

-- | Get the positions that are different between the spacer and the FLT3
-- sequence
otherSpacerPositions :: Position
                     -> Position
                     -> C.ByteString
                     -> Substring
                     -> [Position]
otherSpacerPositions (Position pos) (Position p2) common (Substring spacer)
    | C.null common || length commonPos > C.length spacer =
        fmap Position spacerPos
    | otherwise                                           =
        fmap Position
            . Set.toList
            . Set.difference (Set.fromList spacerPos)
            . Set.fromList
            $ commonPos
  where
    commonPos   = [commonStart .. commonStart + C.length common - 1]
    commonStart = (+ pos) . head . nonOverlappingIndices common $ spacer
    spacerPos   = [pos .. p2 - 1]

-- | Find the string inbetween two indices of the duplication
inbetweenSubstring :: Length -> Position -> Position -> Query -> Substring
inbetweenSubstring (Length n) (Position x) (Position y) =
    Substring . C.take (y - x - n) . C.drop (x + n) . unQuery

-- | Change the query to be identical with the one duplication position
mutateQuery :: Position -> Substring -> Query -> Query
mutateQuery (Position p) (Substring s) (Query q) =
    Query . C.concat $ [beginning, s, end]
  where
    beginning = C.take p q
    end       = C.drop (p + C.length s) $ q

-- | The FLT3 Exon 14 sequence
flt3Exon14 :: String
flt3Exon14 = "CAAACTCTAAATTTTCTCTTGGAAACTCCCATTTGAGATCATATTCATATTCTCTGAAATCAACGTAGAAGTACTCATTATCTGAGGAGCCGGTCACCTGTACCATCTGTAGCTGGCTTTCATACCTAAATTG"

flt3Exon14RevCompl :: String
flt3Exon14RevCompl = "CAATTTAGGTATGAAAGCCAGCTACAGATGGTACAGGTGACCGGCTCCTCAGATAATGAGTACTTCTACGTTGATTTCAGAGAATATGAATATGATCTCAAATGGGAGTTTCCAAGAGAAAATTTAGAGTTTG"
