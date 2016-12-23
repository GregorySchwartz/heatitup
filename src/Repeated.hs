{- Repeated
Gregory W. Schwartz

Collections the functions pertaining to finding the longest repeated substring
-}

{-# LANGUAGE BangPatterns #-}

module Repeated
    ( longestRepeatedSubstring
    , longestRepeatedSubstringMutations
    ) where

-- Standard
import Data.Maybe
import Data.List
import Data.Ord
import qualified Data.Set as Set
import Control.Monad

-- Cabal
import Safe
import qualified Data.SuffixTree as T
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Search
import Control.Lens

-- Local
import Types
import Utility

longestRepeatedSubstringMutations
    :: Blacklist
    -> Distance
    -> Maybe MinMut
    -> [Position]
    -> T.Alphabet Char
    -> MinSize
    -> Query
    -> Maybe LongestSubstring
longestRepeatedSubstringMutations blacklist distance minMut muts alphabet minSize q = do
    longest <- longestRepeatedSubstring blacklist distance alphabet minSize q

    result <- case (minMut, getNextLongestF longest, getNextLongestB longest) of
                  (Nothing, _, _)       -> Just longest
                  (_, Nothing, Nothing) -> addMuts longest
                  (Just (MinMut mm), forward, backward) ->
                    if substringLen (forBackBetter forward backward)
                     - substringLen longest < mm
                        then addMuts longest
                        else longestRepeatedSubstringMutations
                             blacklist
                             distance
                             minMut
                             (snd . (forBackQuery forward backward) longest $ q)
                             alphabet
                             minSize
                           . fst
                           $ (forBackQuery forward backward) longest q

    return result
  where
    forBackQuery (Just x) Nothing               = newQuery mutateQueryForward
    forBackQuery Nothing (Just x)               = newQuery mutateQueryBackward
    forBackQuery (Just forward) (Just backward) =
        if substringLen forward >= substringLen backward
            then newQuery mutateQueryForward
            else newQuery mutateQueryBackward
    forBackBetter Nothing (Just backward) = backward
    forBackBetter (Just forward) Nothing  = forward
    forBackBetter (Just forward) (Just backward) =
        if substringLen forward >= substringLen backward
            then forward
            else backward
    addMuts = Just
            . LongestSubstring
            . over dupMutations (++ muts)
            . unLongestSubstring
    substringLen      =
        C.length . unSubstring . _dupSubstring . unLongestSubstring
    getNextLongestF l =
        longestRepeatedSubstring blacklist distance alphabet minSize
            . fst
            . newQuery mutateQueryForward l
            $ q
    getNextLongestB l =
        longestRepeatedSubstring blacklist distance alphabet minSize
            . fst
            . newQuery mutateQueryBackward l
            $ q
    newQuery f (LongestSubstring l) = f
                                      (head . _dupLocations $ l)
                                      (last . _dupLocations $ l)
                                      (_dupSubstring l)

-- | Mutate the query by replacing a location in the query with the new
-- substring
mutateQueryForward :: Position -> Position -> Substring -> Query -> (Query, [Position])
mutateQueryForward (Position i) (Position j) (Substring s) (Query q) =
    if (i + C.length s >= j) || (j + C.length s >= C.length q)
        then (Query q, [])
        else ( Query . C.concat $ [beginning, new, middle, new, end]
             , [Position $ i + C.length s, Position $ j + C.length s]
             )
  where
    beginning = C.take i q
    middle    = C.take (j - (i + C.length s + 1))
              . C.drop (i + C.length s + 1)
              $ q
    end       = C.drop (j + C.length s + 1) $ q
    new       = C.append s (C.take 1 . C.drop (j + C.length s) $ q)

-- | Mutate the query by replacing a location in the query with the new
-- substring
mutateQueryBackward :: Position -> Position -> Substring -> Query -> (Query, [Position])
mutateQueryBackward (Position i) (Position j) (Substring s) (Query q) =
    if i == 0 || j <= i + C.length s - 1
        then (Query q, [])
        else ( Query . C.concat $ [beginning, new, middle, new, end]
             , [Position $ i - 1, Position $ j - 1]
             )
  where
    beginning = C.take (i - 1) q
    middle    = C.take (j - (i + C.length s) - 1) . C.drop (i + C.length s) $ q
    end       = C.drop (j + C.length s) $ q
    new       = C.append (C.take 1 . C.drop (i - 1) $ q) s

-- | Return the longest repeated substring of a list, specifically with an
-- alphabet
longestRepeatedSubstring
    :: Blacklist
    -> Distance
    -> T.Alphabet Char
    -> MinSize
    -> Query
    -> Maybe LongestSubstring
longestRepeatedSubstring blacklist distance alphabet (MinSize minSize) (Query q) =
    longestNonOverlap (Query q)
        . filter checkThresholds
        . sortBy (comparing snd)
        . substringRankings
        . T.constructWith alphabet
        . C.unpack
        . (flip C.snoc '$')
        $ q
  where
    checkThresholds x =
      ((>= minSize) . C.length . unSubstring . fst $ x)
        && ( not
           . itdFalsePositive blacklist distance
           . C.unpack
           . unSubstring
           . fst
           $ x
           )

-- | Get the longest non overlapping substring
longestNonOverlap :: Query -> [(Substring, Int)] -> Maybe LongestSubstring
longestNonOverlap q =
    fmap (LongestSubstring . fst)
        . maximumByMay (comparing snd)
        . filter (not . isOverlapping . fst)
        . fmap (\(!s, !n) -> (makeDuplication q s, n))

-- | Get the non overlapping indices of a substring
makeDuplication :: Query -> Substring -> Duplication
makeDuplication (Query q) (Substring s) =
    Duplication
    (Substring s)
    headLastDup
    []
  where
    headLastDup = case [headMay dups, lastMay dups] of
                      [Just x, Just y] -> if x == y then [] else [x, y]
                      _                -> []
    dups = fmap Position
         . Set.toList
         . Set.fromList
         . flip nonOverlappingIndices q
         $ s

-- | Check if a substring is overlapping
isOverlapping :: Duplication -> Bool
isOverlapping x = null . drop 1 . nub . _dupLocations $ x

-- | Get the deepest substring in a suffix tree
substringRankings :: T.STree Char -> [(Substring, Int)]
substringRankings = map (over _1 (Substring . C.pack)) . go ([], 0)
  where
    go acc T.Leaf               = [acc]
    go acc@(!x, !n) (T.Node es) = concatMap (check acc) es
    check acc (_, T.Leaf)       = [acc]
    check (!x, !n) (!p, !tree)  =
        go (x ++ T.prefix p, n + (length . T.prefix) p) tree
