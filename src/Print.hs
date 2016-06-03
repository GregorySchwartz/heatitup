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
printITD :: Label -> FastaSequence -> Classification -> ITD -> PrintITD
printITD (Label l) fs classification itd =
    PrintITD { label           = l
             , fHeader         = fastaHeader fs
             , dSubstring      = fromMaybe ""
                               . fmap (unSubstring . _dupSubstring)
                               . _duplication
                               $ itd
             , dLocations      = fromMaybe ""
                               . fmap (possToString . _dupLocations)
                               . _duplication
                               $ itd
             , dMutations      = fromMaybe ""
                               . fmap (possToString . _dupMutations)
                               . _duplication
                               $ itd
             , sSubstring      = fromMaybe ""
                               . fmap (unSubstring . _spacerSubstring)
                               . _spacer
                               $ itd
             , sLocation       =
                 fromMaybe ""
                    . fmap (posToString . _spacerLocation)
                    . _spacer
                    $ itd
             , sOtherLocations = fromMaybe ""
                               . fmap (possToString . _spacerOtherLocations)
                               . _spacer
                               $ itd
             , classification  = C.pack . show $ classification
             }

-- | Convert a list of positions to a string. Add 1 to them because DNA is
-- 1 indexed
possToString :: [Position] -> C.ByteString
possToString = C.intercalate "/" . fmap posToString

-- | Convert a position to a string. Add 1 to them because DNA is
-- 1 indexed
posToString :: Position -> C.ByteString
posToString = CL.toStrict . CS.show . (+ 1) . unPosition
