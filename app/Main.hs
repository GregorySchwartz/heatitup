{- find-duplication
Gregory W. Schwartz

Find duplications a sequence.
-}

{-# LANGUAGE BangPatterns #-}

module Main where

-- Standard
import Data.Maybe
import qualified Data.Map.Strict as Map
import qualified System.IO as IO
import Control.Monad

-- Cabal
import qualified Data.Vector as V
import qualified Data.ByteString.Char8 as C
import Pipes
import qualified Pipes.Prelude as P
import qualified Pipes.ByteString as PB
import Pipes.Csv
import Data.Fasta.ByteString
import qualified Diagrams.Backend.PGF as PGF
import Diagrams.TwoD.Size (mkWidth, mkHeight)
import Options.Applicative
import Data.Fasta.ByteString

-- Local
import Types
import Utility
import Load
import Repeated
import Spacer
import Classify
import Plot
import Print

-- | Command line arguments
data Options = Options { input           :: Maybe String
                       , refInput        :: String
                       , output          :: Maybe String
                       , outputPlot      :: Maybe String
                       , outputLabel     :: String
                       , refField        :: Int
                       , minSize         :: Int
                       , gaussWindow     :: Int
                       , gaussTime       :: Double
                       , gaussThreshold  :: Double
                       , minMut          :: Maybe Int
                       , distance        :: Int
                       , revComplFlag    :: Bool
                       }

-- | Command line options
options :: Parser Options
options = Options
      <$> optional ( strOption
          ( long "input"
         <> short 'i'
         <> metavar "FILE"
         <> help "The input file"
          )
        )
      <*> strOption
          ( long "reference-input"
         <> short 'I'
         <> metavar "FILE"
         <> help "The input file containing the reference sequences to compare\
                 \ to. The first entry in the field must be the accession and\
                 \ match the requested field from reference-field for the\
                 \ input."
          )
      <*> optional ( strOption
          ( long "output"
         <> short 'o'
         <> metavar "FILE"
         <> help "The output file"
          )
        )
      <*> optional ( strOption
          ( long "output-plot"
         <> short 'O'
         <> metavar "FILE"
         <> help "The output file for the plot. Each new plot gets a new number\
                 \ on it: output_1.svg, output_2.svg, etc. Each plot uses the\
                 \ first entry in the fasta header as the label."
          )
        )
      <*> strOption
          ( long "label"
         <> short 'l'
         <> metavar "FILE"
         <> value ""
         <> help "The label to use in the label column for the output"
          )
      <*> option auto
          ( long "reference-field"
         <> short 'f'
         <> metavar "[1] | INT"
         <> value 1
         <> help "The field in each input header that contains the reference\
                 \ accession number to compare to."
          )
      <*> option auto
          ( long "min-size"
         <> short 's'
         <> metavar "[15] | INT"
         <> value 15
         <> help "The minimum size of a duplication"
          )
      <*> option auto
          ( long "gaussian-window"
         <> short 'w'
         <> metavar "[4] | Double"
         <> value 3
         <> help "The window for the discrete gaussian kernel atypical spacer\
                 \ determination"
          )
      <*> option auto
          ( long "gaussian-time"
         <> short 't'
         <> metavar "[2] | Double"
         <> value 2
         <> help "The time for the discrete gaussian kernel atypical spacer\
                 \ determination"
          )
      <*> option auto
          ( long "gaussian-threshold"
         <> short 'T'
         <> metavar "[0.4] | Double"
         <> value 0.4
         <> help "The cutoff to be considered a mutation for the discrete\
                 \ gaussian kernel atypical spacer determination"
          )
      <*> optional ( option auto
          ( long "min-mutations"
         <> short 'm'
         <> metavar "INT"
         <> help "The minimum number of nucleotides between mutations"
          )
        )
      <*> option auto
          ( long "levenshtein-distance"
         <> short 'L'
         <> metavar "[2] | INT"
         <> value 2
         <> help "The minimum Levenshtein distance to the false positive\
                 \ checker. If the distance to the false positive string\
                 \ is less than this number, the duplication is considered\
                 \ a false positive."
          )
      <*> switch
          ( long "reverse-complement"
         <> short 'r'
         <> help "Whether the sequences are the reverse complement FLT3 Exon 14"
          )

plotITDM :: Options -> (Int, (ITD, FastaSequence)) -> IO (ITD, FastaSequence)
plotITDM opts (!count, (!itd, !fs)) = do
    let savefile = (++ "_" ++ show count ++ ".pdf")
                 . fromMaybe ""
                 . outputPlot
                 $ opts

    unless (isNothing . outputPlot $ opts)
        . PGF.renderPGF savefile (mkHeight (60 :: Double))
        . plotITD fs itd
        . Query
        . fastaSeq
        $ fs

    return (itd, fs)

mainFunc :: Options -> IO ()
mainFunc opts = do
    hIn  <- case input opts of
                Nothing  -> return IO.stdin
                (Just x) -> IO.openFile x IO.ReadMode
    hOut <- case output opts of
                Nothing  -> return IO.stdout
                (Just x) -> IO.openFile x IO.WriteMode

    IO.withFile (refInput opts) IO.ReadMode $ \hRefIn -> do

        refMap <- fmap (toReferenceMap (Field 1))
                . readReference
                $ hRefIn

        let getDup fs = ( longestRepeatedSubstringMutations
                          (fmap MinMut . minMut $ opts)
                          []
                          "ATCG"
                          (MinSize . minSize $ opts)
                          . Query
                          . fastaSeq
                          $ fs
                        , fs
                        )
            getSpace (!dup, !fs) =
                ( ITD { _duplication = fmap unLongestSubstring dup
                      , _spacer      =
                          join
                            . fmap ( flip ( getSpacer
                                            (Window $ gaussWindow opts)
                                            (Time $ gaussTime opts)
                                            (Threshold $ gaussThreshold opts)
                                            refSeq
                                          )
                                          (Query . fastaSeq $ fs)
                                   . unLongestSubstring
                                   )
                            $ dup
                      }
                , fs
                )
              where
                refSeq = ReferenceSeq
                       . fastaSeq
                       . fromMaybe ( error
                                   . (++) "Reference accession field not found in: "
                                   . C.unpack
                                   . fastaHeader
                                   $ fs
                                   )
                       . Map.lookup ( Accession
                                    . getField (refField opts) '|'
                                    $ fs
                                    )
                       . unReferenceMap
                       $ refMap
            falsePositiveITDCheck (!itd, !fs) =
                if not
                 . itdFalsePositive
                    (revComplFlag opts)
                    (Distance $ distance opts)
                 $ itd
                    then (itd, fs)
                    else (itd { _duplication = Nothing, _spacer = Nothing }, fs)
            getClass (!itd, !fs)     = (classifyITD itd, itd, fs)
            printRow (!c, !itd, !fs) =
                printITD (Label . C.pack . outputLabel $ opts) fs c itd
            headerOrder              = V.fromList [ C.pack "label"
                                                  , C.pack "fHeader"
                                                  , C.pack "fSequence"
                                                  , C.pack "dSubstring"
                                                  , C.pack "dLocations"
                                                  , C.pack "dMutations"
                                                  , C.pack "sSubstring"
                                                  , C.pack "sLocation"
                                                  , C.pack "sOtherLocations"
                                                  , C.pack "classification"
                                                  ]

        runEffect $ ( ( P.zip (each [1..])
                            ( pipesFasta (PB.fromHandle hIn)
                          >-> P.map (getSpace . getDup)
                            )
                      )
                    >-> P.mapM (plotITDM opts)
                    >-> P.map (printRow . getClass . falsePositiveITDCheck)
                    >-> encodeByName headerOrder
                    )
                >-> PB.toHandle hOut

        -- Finish up by closing file if written
        unless (null . input $ opts) (IO.hClose hIn)
        unless (null . output $ opts) (IO.hClose hOut)

main :: IO ()
main = execParser opts >>= mainFunc
  where
    opts = info (helper <*> options)
      ( fullDesc
     <> progDesc "Finds duplications in a sequence"
     <> header "find-duplication, Gregory W. Schwartz" )
