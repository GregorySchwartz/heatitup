{- Print
Gregory W. Schwartz

Collections the functions pertaining to printing the ITD output.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Print
    ( printITD
    ) where

-- Standard
import Data.Maybe

-- Cabal
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Text.Show.ByteString as CS
import Data.Fasta.ByteString

-- Local
import Types

-- | Put the ITD type in a record to be converted to a csv row
printITD :: Label
         -> Position
         -> FastaSequence
         -> Classification
         -> ITD
         -> PrintITD
printITD (Label l) p fs classification itd =
    PrintITD { label           = l
             , fHeader         = fastaHeader fs
             , fSequence       = fastaSeq fs
             , dSubstring      = fromMaybe ""
                               . fmap (unSubstring . _dupSubstring)
                               . _duplication
                               $ itd
             , dLocations      = fromMaybe ""
                               . fmap (possToString p . _dupLocations)
                               . _duplication
                               $ itd
             , dMutations      = fromMaybe ""
                               . fmap (possToString p . _dupMutations)
                               . _duplication
                               $ itd
             , sSubstring      = fromMaybe ""
                               . fmap (unSubstring . _spacerSubstring)
                               . _spacer
                               $ itd
             , sLocation       =
                 fromMaybe ""
                    . fmap (posToString p . _spacerLocation)
                    . _spacer
                    $ itd
             , sOtherLocations = fromMaybe ""
                               . fmap (possToString p . _spacerOtherLocations)
                               . _spacer
                               $ itd
             , classification  = C.pack . show $ classification
             }

-- | Convert a list of positions to a string.
possToString :: Position -> [Position] -> C.ByteString
possToString p = C.intercalate "/" . fmap (posToString p)

-- | Convert a position to a string. Add the starting point of the read
-- (converts to 1 indexed).
posToString :: Position -> Position -> C.ByteString
posToString p = CL.toStrict . CS.show . unPosition . (+ p)
