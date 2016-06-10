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
import Data.Ord
import Data.List
import qualified Data.Set as Set
import Debug.Trace

-- Cabal
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Search
import Data.List.CommonSubstring
import Safe
import Control.Lens

-- Local
import Types
import Utility
import Diffusion

getSpacer :: Bool
          -> Window
          -> Time
          -> Threshold
          -> Duplication
          -> Query
          -> Maybe Spacer
getSpacer revCompl
          window
          time
          threshold
          (Duplication { _dupSubstring = s, _dupLocations = [p1, p2] })
          q
    | C.null . unSubstring $ spacer = Nothing
    | otherwise                     =
        Just $
            Spacer { _spacerSubstring      = spacer
                   , _spacerLocation       = spacerPos
                   , _spacerOtherLocations = otherSpacerPositionsDiffusion
                                             window
                                             time
                                             threshold
                                             spacerPos
                                             p1
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

-- | Get the most similar sequence to a base sequence from the left or
-- right substring, along with the base fragment
minHammingLeftRight ::
    C.ByteString
        -> Substring
        -> Substring 
        -> Either (Substring, C.ByteString) (Substring, C.ByteString)
minHammingLeftRight base (Substring s) (Substring spacer) =
    either (Left . (Substring leftSeq,)) (Right . (Substring rightSeq,))
        . minimumBy (comparing $ either (hamming leftSeq) (hamming rightSeq))
        . concat
        $ [ fmap (Left . unSubstring) . baseFragments $ leftSeq
          , fmap (Right . unSubstring) . baseFragments $ rightSeq
          ]
  where
    baseFragments xs = fragmentSequenceFiller (Window . C.length $ xs) $ base
    leftSeq          = getLeftRightFull LeftP spacer s
    rightSeq         = getLeftRightFull RightP spacer s

-- | Get the positions that are different between the spacer and the FLT3
-- sequence
otherSpacerPositionsDiffusion :: Window
                              -> Time
                              -> Threshold
                              -> Position
                              -> Position
                              -> Position
                              -> C.ByteString
                              -> Substring
                              -> Substring
                              -> [Position]
otherSpacerPositionsDiffusion window
                              time
                              (Threshold gaussThresh)
                              (Position spacerPos)
                              (Position p1)
                              (Position p2)
                              base
                              (Substring s)
                              (Substring spacer) =
    fmap Position
        . Set.toList
        . Set.intersection (Set.fromList spacerPoss)
        . Set.fromList
        . either (fmap (+ p1)) (fmap (+ spacerPos))
        . over both atypicalPositions
        $ minHamming
  where
    atypicalPositions =
        fmap fst
            . filter snd
            . zip [0..]
            . fmap (>= gaussThresh)
            . unSignal
            . diffusedSeq
    diffusedSeq (joinedSubstring, baseFragment) =
        diffuse window time
            . mutationSignal baseFragment
            $ joinedSubstring 
    minHamming = minHammingLeftRight base (Substring s) (Substring spacer)
    spacerPoss  = [spacerPos .. p2 - 1]

-- | Get the positions that are different between the spacer and the FLT3
-- sequence
otherSpacerPositions :: MinSize
                     -> Consecutive
                     -> Position
                     -> Position
                     -> Position
                     -> C.ByteString
                     -> Substring
                     -> Substring
                     -> [Position]
otherSpacerPositions (MinSize minAtypicalSize)
                     consecutive
                     (Position spacerPos)
                     (Position p1)
                     (Position p2)
                     base
                     (Substring s)
                     (Substring spacer)
    | consecutiveSpacerFalsePositive consecutive base
    . Substring
    . getLeftRightPortion LeftP spacer
    $ s
    = []
    | consecutiveSpacerFalsePositive consecutive base
    . Substring
    . getLeftRightPortion RightP spacer
    $ s
    = []
    | otherwise                                                                =
        fmap Position
            . Set.toList
            . Set.difference (Set.fromList spacerPoss)
            . Set.intersection (Set.fromList spacerPoss)
            . Set.fromList
            . commonPos
            $ commonStart
  where
    commonPos Nothing       = []
    commonPos (Just (Left (Length l)))  = [p1 + C.length s - 1 .. p2 - l - 1]
    commonPos (Just (Right (Length l))) = [spacerPos + l .. p2 - 1]
    commonStart = headMay
                . catMaybes
                . fmap ( leftOrRightFound
                         (Substring base)
                         (Substring spacer)
                         (Substring s)
                       . Length
                       )
                $ [0 .. minLen]
    minLen      = if C.length spacer <= minAtypicalSize
                      then (-1)
                      else C.length spacer - minAtypicalSize
    spacerPoss  = [spacerPos .. p2 - 1]

-- | Whether the left or right substring is found in the sequence
leftOrRightFound :: Substring
                 -> Substring
                 -> Substring
                 -> Length
                 -> Maybe (Either Length Length)
leftOrRightFound (Substring base) (Substring spacer) (Substring s) (Length l) =
    if C.isInfixOf newString1 base
        then Just . Left . Length $ l
        else if C.isInfixOf newString2 base 
            then Just . Right . Length $ l
            else Nothing
  where
    newString1 = C.reverse . C.drop l . C.reverse $ original1
    newString2 = C.drop l original2
    original1  = getLeftRightPortion LeftP spacer s
    original2  = getLeftRightPortion RightP spacer s

-- | Get the string plus a the entirety of another string and
-- concatenate them
getLeftRightFull :: LeftRightPortion
                 -> C.ByteString
                 -> C.ByteString
                 -> C.ByteString
getLeftRightFull LeftP x y  = C.append y x
getLeftRightFull RightP x y = C.append x y

-- | Get the string plus a portion (1 / 3) of another string and
-- concatenate them
getLeftRightPortion :: LeftRightPortion
                    -> C.ByteString
                    -> C.ByteString
                    -> C.ByteString
getLeftRightPortion LeftP x y  =
    C.append (C.drop (2 * (C.length y `div` 3)) y) x
getLeftRightPortion RightP x y = C.append x . C.take (C.length y `div` 3) $ y

-- | Find the string inbetween two indices of the duplication
inbetweenSubstring :: Length -> Position -> Position -> Query -> Substring
inbetweenSubstring (Length n) (Position x) (Position y) =
    Substring . C.take (y - x - n) . C.drop (x + n) . unQuery

-- | The FLT3 Exon 14 sequence
flt3Exon14 :: String
flt3Exon14 = "CAAACTCTAAATTTTCTCTTGGAAACTCCCATTTGAGATCATATTCATATTCTCTGAAATCAACGTAGAAGTACTCATTATCTGAGGAGCCGGTCACCTGTACCATCTGTAGCTGGCTTTCATACCTAAATTG"

flt3Exon14RevCompl :: String
flt3Exon14RevCompl = "CAATTTAGGTATGAAAGCCAGCTACAGATGGTACAGGTGACCGGCTCCTCAGATAATGAGTACTTCTACGTTGATTTCAGAGAATATGAATATGATCTCAAATGGGAGTTTCCAAGAGAAAATTTAGAGTTTG"
