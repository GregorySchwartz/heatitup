{- Print
Gregory W. Schwartz

Collections the functions pertaining to the loading of fasta files.
-}

module Load
    ( readReference
    , toReferenceMap
    ) where

-- Standard
import qualified Data.Map.Strict as Map
import qualified System.IO as IO

-- Cabal
import Pipes
import qualified Pipes.Prelude as P
import qualified Pipes.ByteString as PB
import Data.Fasta.ByteString

-- Local
import Types

-- | Load the input file containing the reference sequences.
readReference :: IO.Handle -> IO [FastaSequence]
readReference hRefIn =
    runEffect $ P.toListM . pipesFasta . PB.fromHandle $ hRefIn

-- | Convert a list of reference sequences to a map of their accessions
toReferenceMap :: Field -> [FastaSequence] -> ReferenceMap
toReferenceMap (Field x) =
    ReferenceMap
        . Map.unions
        . fmap (\fs -> Map.singleton (Accession . getField x '|' $ fs) fs)
