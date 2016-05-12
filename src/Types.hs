{- Types
Gregory W. Schwartz

Collections the types used in the program
-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

-- Standard

-- Cabal
import qualified Data.ByteString.Char8 as C
import Control.Lens

-- Local

-- Algebraic
newtype Position         = Position { unPosition :: Int }
                           deriving (Eq, Show, Num)
newtype LongestSubstring = LongestSubstring
                           { unLongestSubstring :: Duplication }
                           deriving (Show)
newtype Query     = Query { unQuery :: C.ByteString } deriving (Show)
newtype Substring = Substring { unSubstring :: C.ByteString }
                    deriving (Show)
newtype Length    = Length Int deriving (Show)
newtype MinSize   = MinSize Int
newtype MinMut    = MinMut { unMinMut :: Int }
newtype MaxMut    = MaxMut { unMaxMut :: Int } deriving (Eq, Ord)
newtype Window    = Window Int

data Classification = Typical | Atypical | Unknown deriving (Show)

data Duplication = Duplication { _dupSubstring        :: Substring
                               , _dupLocations        :: [Position]
                               , _dupMutations        :: [Position]
                               }
                    deriving (Show)
makeLenses ''Duplication

data Spacer = Spacer { _spacerSubstring      :: Substring
                     , _spacerLocation       :: Position
                     , _spacerOtherLocations :: [Position]
                     }
              deriving (Show)
makeLenses ''Spacer

data ITD = ITD { _duplication :: Maybe Duplication
               , _spacer      :: Maybe Spacer
               }
           deriving (Show)
makeLenses ''ITD

-- Basic

-- Advanced
