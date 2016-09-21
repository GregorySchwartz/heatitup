{- Plot
Gregory W. Schwartz

Collections the functions pertaining to plotting the sequence.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Plot
    ( plotITD
    ) where

-- Standard
import Data.Maybe
import Data.List

-- Cabal
import qualified Data.ByteString.Char8 as C
import Data.Colour.SRGB (sRGB24read)
import Diagrams.Prelude
import Data.Fasta.ByteString

-- Local
import Types

foreground p (!xs, !ys)
    | p `elem` xs = square 1 # fc (colors "darkred") # lc (colors "darkred")
    | p `elem` ys = square 1 # fc (colors "darkcyan") # lc (colors "darkcyan")
    | otherwise   = mempty

background p (!xs, !ys)
    | p `elem` xs = colors "darkblue"
    | p `elem` ys = colors "darkmagenta"
    | otherwise   = colors "white"

plotNucleotide nuc mut backG = text nuc # fc (colors "black")
                            <> mut
                            <> square 1 # fc backG # lc backG

plotITD fs itd = hcat
               . (:) ( text label
                    <> rect (genericLength label) 1 # lw none
                     )
               . fmap (uncurry plotNuc)
               . zip (fmap Position [0..])
               . C.unpack
               . unQuery
  where
    label       = takeWhile (/= '|') . C.unpack . fastaHeader $ fs
    dupLen      = fromMaybe 0
                . fmap (C.length . unSubstring . _dupSubstring)
                . _duplication
                $ itd
    plotNuc p x =
        plotNucleotide
            (x : [])
            ( foreground p ( (fromMaybe []
                             . fmap _dupMutations
                             . _duplication
                             $ itd
                             )
                           , ( fromMaybe []
                             . fmap _spacerOtherLocations
                             . _spacer
                             $ itd
                             )
                           )
            )
            ( background p ( fromMaybe ([], [])
                           . fmap ( (\[x, y] -> (dupPositions x, dupPositions y))
                                              . _dupLocations
                                  )
                           . _duplication
                           $ itd
                           )
            )
    dupPositions (Position x) = fmap Position [x .. x + dupLen - 1]

colors :: String -> Colour Double
colors "background"  = sRGB24read "#282828"
colors "foreground"  = sRGB24read "#ebdbb2"
colors "black"       = sRGB24read "#282828"
colors "darkgrey"    = sRGB24read "#928374"
colors "darkred"     = sRGB24read "#cc241d"
colors "red"         = sRGB24read "#fb4934"
colors "darkgreen"   = sRGB24read "#98971a"
colors "green"       = sRGB24read "#b8bb26"
colors "darkyellow"  = sRGB24read "#d79921"
colors "yellow"      = sRGB24read "#fabd2f"
colors "darkblue"    = sRGB24read "#458588"
colors "blue"        = sRGB24read "#83a598"
colors "brightblue"  = sRGB24read "#2e9ef4"
colors "darkmagenta" = sRGB24read "#b16286"
colors "magenta"     = sRGB24read "#d3869b"
colors "darkcyan"    = sRGB24read "#689d6a"
colors "cyan"        = sRGB24read "#8ec07c"
colors "lightgrey"   = sRGB24read "#a89984"
colors "white"       = sRGB24read "#ebdbb2"
