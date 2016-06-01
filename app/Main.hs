{- find-duplication
Gregory W. Schwartz

Find duplications in the FLT3 exon 14 sequence.
-}

{-# LANGUAGE BangPatterns #-}

module Main where

-- Standard
import Data.Maybe
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

-- Local
import Types
import Utility
import Repeated
import Spacer
import Classify
import Plot
import Print

-- | Command line arguments
data Options = Options { input           :: Maybe String
                       , output          :: Maybe String
                       , outputPlot      :: Maybe String
                       , outputLabel     :: String
                       , minSize         :: Int
                       , minAtypicalSize :: Int
                       , minMut          :: Maybe Int
                       , revCompl        :: Bool
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
                 \ on it: output_1.svg, output_2.svg, etc."
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
          ( long "min-size"
         <> short 's'
         <> metavar "INT"
         <> value 5
         <> help "The minimum size of a duplication"
          )
      <*> option auto
          ( long "min-atypical-size"
         <> short 'S'
         <> metavar "INT"
         <> value 5
         <> help "The minimum size of spacer commonality to be considered\
                 \ not atypical"
          )
      <*> optional ( option auto
          ( long "min-mutations"
         <> short 'm'
         <> metavar "INT"
         <> help "The minimum number of nucleotides between mutations"
          )
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
                                        (revCompl opts)
                                        (MinSize $ minAtypicalSize opts)
                                      )
                                      (Query . fastaSeq $ fs)
                               . unLongestSubstring
                               )
                        $ dup
                  }
            , fs
            )
        filterFalsePositive (!itd, !fs) =
            not . itdFalsePositive (revCompl opts) $ itd
        getClass (!itd, !fs)     = (classifyITD itd, itd, fs)
        printRow (!c, !itd, !fs) =
            printITD (Label . C.pack . outputLabel $ opts) fs c itd
        headerOrder              = V.fromList [ C.pack "label"
                                              , C.pack "fHeader"
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
                >-> P.filter filterFalsePositive
                >-> P.map (printRow . getClass)
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
