{- Diffusion
Gregory W. Schwartz

Collections the functions pertaining to the discrete general diffusion
equation, or the discrete Gaussian kernel
-}

{-# LANGUAGE BangPatterns #-}

module Diffusion
    ( diffuse
    , mutationSignal
    ) where

-- Standard
import Data.Maybe

-- Cabal
import qualified Data.Vector as V
import qualified Data.ByteString.Char8 as C

-- Local
import Types
import Utility

-- | Diffuse a 1 dimensional discrete signal
diffuse :: Window -> Time -> Signal -> Signal
diffuse _ _ (Signal []) = Signal []
diffuse (Window w) (Time t) (Signal xs) = Signal . V.toList . V.imap go $ vec
  where
    vec    = V.fromList xs
    go i x =
        sum
            . map ( \n -> (* g (fromIntegral n))
                          . fromMaybe 0
                          . (V.!?) vec
                          . fromIntegral
                          $ i - n
                  )
            $ [(- w) .. w]
    g n    = (1 / (sqrt (2 * pi * t))) * (exp ((- n ^ 2) / (2 * t)))

-- | Convert a hamming comparison to a signal
mutationSignal :: C.ByteString -> Substring -> Signal
mutationSignal base = Signal
                    . fmap (\x -> if x then 0 else 1)
                    . hammingList base
                    . unSubstring
