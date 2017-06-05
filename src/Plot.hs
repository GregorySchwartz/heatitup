{- Plot
Gregory W. Schwartz

Collections the functions pertaining to plotting the sequence.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

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

foreground colors p (!xs, !ys)
    | p `elem` xs = square 1 # fc (_colorMut colors) # lc (_colorMut colors)
    | p `elem` ys = square 1 # fc (_colorSpacer colors) # lc (_colorSpacer colors)
    | otherwise   = mempty

background colors p (!xs, !ys)
    | p `elem` xs = _colorDupL colors
    | p `elem` ys = _colorDupR colors
    | otherwise   = _colorBackground colors

plotNucleotide colors nuc mut backG =
    text nuc # fc (_colorForeground colors)
        <> mut
        <> square 1 # fc backG # lc backG

plotITD
    :: _
    => Colors -> FastaSequence -> ITD -> Types.Query -> QDiagram b V2 n Any
plotITD colors fs itd = hcat
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
            colors
            (x : [])
            ( foreground colors p ( ( fromMaybe []
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
            ( background
                colors
                p
                ( fromMaybe ([], [])
                . fmap ( (\[x, y] -> (dupPositions x, dupPositions y))
                                   . _dupLocations
                       )
                . _duplication
                $ itd
                )
            )
    dupPositions (Position x) = fmap Position [x .. x + dupLen - 1]

gruvColors :: String -> Colour Double
gruvColors "background"  = sRGB24read "#282828"
gruvColors "foreground"  = sRGB24read "#ebdbb2"
gruvColors "black"       = sRGB24read "#282828"
gruvColors "darkgrey"    = sRGB24read "#928374"
gruvColors "darkred"     = sRGB24read "#cc241d"
gruvColors "red"         = sRGB24read "#fb4934"
gruvColors "darkgreen"   = sRGB24read "#98971a"
gruvColors "green"       = sRGB24read "#b8bb26"
gruvColors "darkyellow"  = sRGB24read "#d79921"
gruvColors "yellow"      = sRGB24read "#fabd2f"
gruvColors "darkblue"    = sRGB24read "#458588"
gruvColors "blue"        = sRGB24read "#83a598"
gruvColors "brightblue"  = sRGB24read "#2e9ef4"
gruvColors "darkmagenta" = sRGB24read "#b16286"
gruvColors "magenta"     = sRGB24read "#d3869b"
gruvColors "darkcyan"    = sRGB24read "#689d6a"
gruvColors "cyan"        = sRGB24read "#8ec07c"
gruvColors "lightgrey"   = sRGB24read "#a89984"
gruvColors "white"       = sRGB24read "#ebdbb2"
