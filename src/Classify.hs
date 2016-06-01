{- Classify
Gregory W. Schwartz

Classify an ITD.
-}

module Classify
    ( classifyITD
    ) where

-- Standard

-- Cabal

-- Local
import Types

-- | Classify an ITD
classifyITD :: ITD -> Classification
classifyITD (ITD { _duplication = Nothing }) = Normal
classifyITD (ITD { _spacer = Nothing})       = Typical
classifyITD (ITD { _spacer = (Just s)}) =
    if not . null . _spacerOtherLocations $ s then Atypical else Typical
