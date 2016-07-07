{- Types
Gregory W. Schwartz

Collections the types used in the program
-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where

-- Standard
import GHC.Generics
import qualified Data.Map.Strict as Map

-- Cabal
import qualified Data.ByteString.Char8 as C
import Data.Csv
import Control.Lens
import Data.Fasta.ByteString

-- Local

-- Algebraic
newtype Position         = Position { unPosition :: Int }
                           deriving (Eq, Show, Num)
newtype LongestSubstring = LongestSubstring
                            { unLongestSubstring :: Duplication }
                            deriving (Show)
newtype Query        = Query { unQuery :: C.ByteString } deriving (Show)
newtype Substring    = Substring { unSubstring :: C.ByteString }
                       deriving (Eq, Show)
newtype Length       = Length Int deriving (Show)
newtype Distance     = Distance Int deriving (Show)
newtype Threshold    = Threshold Double deriving (Show)
newtype Percent      = Percent Double deriving (Show)
newtype Consecutive  = Consecutive Int deriving (Show)
newtype Label        = Label C.ByteString
newtype MinSize      = MinSize Int
newtype MinMut       = MinMut { unMinMut :: Int }
newtype MaxMut       = MaxMut { unMaxMut :: Int } deriving (Eq, Ord)
newtype Window       = Window Int
newtype Signal       = Signal { unSignal :: [Double] } deriving (Eq, Ord, Show)
newtype Time         = Time Double deriving (Show)
newtype Field        = Field Int deriving (Show)
newtype Accession    = Accession C.ByteString deriving (Show, Eq, Ord)
newtype ReferenceSeq = ReferenceSeq C.ByteString deriving (Show)
newtype ReferenceMap = ReferenceMap { unReferenceMap
                                   :: (Map.Map Accession FastaSequence)
                                    }
                       deriving (Show)

data Classification = Typical | Atypical | Normal deriving (Eq, Ord, Read, Show)

data LeftRightPortion = LeftP | RightP deriving (Show)

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

data PrintITD = PrintITD { label           :: C.ByteString
                         , fHeader         :: C.ByteString
                         , fSequence       :: C.ByteString
                         , dSubstring      :: C.ByteString
                         , dLocations      :: C.ByteString
                         , dMutations      :: C.ByteString
                         , sSubstring      :: C.ByteString
                         , sLocation       :: C.ByteString
                         , sOtherLocations :: C.ByteString
                         , classification  :: C.ByteString
                         }
                deriving (Show, Generic)

instance FromNamedRecord PrintITD
instance ToNamedRecord PrintITD

-- Basic
type FileName = String

-- Advanced
