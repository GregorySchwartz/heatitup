{- find-duplication
Gregory W. Schwartz

Find duplications a sequence.
-}

{-# LANGUAGE BangPatterns #-}

module Main where

-- Standard
import Control.Monad
import Data.Maybe
import Data.Bool
import Data.Semigroup ((<>))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified System.IO as IO

-- Cabal
import Control.Lens
import Data.Fasta.ByteString
import Diagrams.TwoD.Size (mkWidth, mkHeight)
import Options.Applicative
import Pipes
import Pipes.Csv
import Safe
import qualified Data.ByteString.Char8 as C
import qualified Data.Vector as V
import qualified Diagrams.Backend.PGF as PGF
import qualified Pipes.ByteString as PB
import qualified Pipes.Prelude as P

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
data Options = Options { input                 :: Maybe String
                       , spacerFlag            :: Bool
                       , refInput              :: Maybe String
                       , blacklistInput        :: Maybe String
                       , output                :: Maybe String
                       , outputPlot            :: Maybe String
                       , outputLabel           :: String
                       , refField              :: Int
                       , posField              :: Maybe Int
                       , ignoreField           :: Maybe Int
                       , inputMinSize          :: Int
                       , gaussWindow           :: Int
                       , gaussTime             :: Double
                       , gaussThreshold        :: Double
                       , inputMinMut           :: Maybe Int
                       , inputDistance         :: Int
                       , refCheckBlacklistFlag :: Bool
                       , refRecBlacklistFlag   :: Bool
                       , minRichness           :: Int
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
      <*> switch
          ( long "spacer"
         <> short 's'
         <> help "Whether to characterize the spacer. Requires\
                 \ reference-input and matching labels for the input and\
                 \ reference-input to compare."
          )
      <*> optional ( strOption
          ( long "reference-input"
         <> short 'I'
         <> metavar "[Nothing] | FILE"
         <> help "The input file containing the reference sequences to compare\
                 \ to. The first entry in the field must be the accession and\
                 \ match the requested field from reference-field for the\
                 \ input. With no supplied file, no spacer will be annotated."
          )
        )
      <*> optional ( strOption
          ( long "blacklist-input"
         <> short 'b'
         <> metavar "[Nothing] | FILE"
         <> help "The input fasta file containing possible false positives --\
                 \ sequences which may be duplicate nucleotides in the\
                 \ reference sequence."
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
                 \ accession number to compare to. Results in an out of bounds\
                 \ if this field does not exist."
          )
      <*> optional ( option auto
          ( long "position-field"
         <> short 'p'
         <> metavar "[Nothing] | INT"
         <> help "The field in each input header that contains the starting\
                 \ position of the read. Added to the annotations. Results\
                 \ in out of bounds if this field does not exist."
          )
        )
      <*> optional ( option auto
          ( long "ignore-field"
         <> short 'g'
         <> metavar "[Nothing] | INT"
         <> help "The field in each input header that contains a 0 or a 1:\
                 \ 0 means to ignore this read (assign as Normal) and 1\
                 \ means to find a duplication in this read.\
                 \ Used for reads where there is known to be no duplication\
                 \ and thus helps remove false positives."
          )
        )
      <*> option auto
          ( long "min-size"
         <> short 'S'
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
                 \ is less than or equal to this number,\
                 \ the duplication is considered\
                 \ a false positive. Compares candidates against each sequence\
                 \ in --blacklist-input"
          )
      <*> switch
          ( long "reference-check-blacklist"
         <> short 'c'
         <> help "Whether to use the reference as a blacklist in addition to\
                 \ the supplied blacklist. That is, we check if the duplication\
                 \ can be found twice or more in the reference input."
          )
      <*> switch
          ( long "reference-recursive-blacklist"
         <> short 'r'
         <> help "Whether to use the reference as a recursive\
                 \ blacklist in addition to\
                 \ the supplied blacklist. That is, the reference sequences\
                 \ are inputed with the same parameters (except distance, which\
                 \ here is 0)\
                 \ to the duplication finder, and those duplications found are\
                 \ added to the blacklist. This process is recursive, executed\
                 \ until no more duplications are found in the reference.\
                 \ Beware, too many blacklist entries can slow down the finder\
                 \ significantly, as each blacklist entry is compared with each\
                 \ candidate."
          )
      <*> option auto
          ( long "min-richness"
         <> short 'R'
         <> metavar "[1] | INT"
         <> value 1
         <> help "The minimum nucleotide richness (number of different types of\
                 \ nucleotides) allowed in the duplication to be considered\
                 \ real. Useful if the user knows that a sequence like\
                 \ \"TTTTTTTTCTTTTTTTTC\" is not likely to be real."
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

getReferenceSeq :: Int -> FastaSequence -> ReferenceMap -> ReferenceSeq
getReferenceSeq field fs = ReferenceSeq
                . fastaSeq
                . fromMaybe ( error
                            . (++) "Reference accession field not found in: "
                            . C.unpack
                            . fastaHeader
                            $ fs
                            )
                . Map.lookup ( Accession
                            . getField field '|'
                            $ fs
                            )
                . unReferenceMap

-- | Unpack a longest substring into a string.
unpackSubstring :: LongestSubstring -> String
unpackSubstring = C.unpack . unSubstring . _dupSubstring . unLongestSubstring

-- | Get a blacklist using a recursive method on the reference sequences. That
-- is, repeatedly exhaust all possibly repeats in the reference sequences.
-- Slower than the reference check if there are lots of sequences in the
-- reference.
refRecBlacklist :: RepeatConfig -> [FastaSequence] -> Blacklist
refRecBlacklist !tempConfig fss =
    case catMaybes longestList of
        [] -> _blacklist tempConfig
        xs -> refRecBlacklist
                ( over blacklist ( Blacklist
                                 . Set.union ( Set.fromList
                                             . fmap unpackSubstring
                                             $ xs
                                             )
                                 . unBlacklist
                                 ) tempConfig
                )
            . catMaybes
            . zipMaybe longestList
            $ fss
    where
    longestList = fmap
                    ( longestRepeatedSubstringMutations tempConfig []
                    . Query
                    . fastaSeq
                    )
                    fss

mainFunc :: Options -> IO ()
mainFunc opts = do
    hIn  <- case input opts of
                Nothing  -> return IO.stdin
                (Just x) -> IO.openFile x IO.ReadMode
    hOut <- case output opts of
                Nothing  -> return IO.stdout
                (Just x) -> IO.openFile x IO.WriteMode

    refMap <- case refInput opts of
                Nothing  -> return Nothing
                (Just x) -> IO.withFile x IO.ReadMode $ \hRefIn ->
                    fmap (Just . toReferenceMap (Field 1))
                        . readFasta
                        $ hRefIn

    suppliedBlacklist <-
        case blacklistInput opts of
            Nothing  -> return . Blacklist $ Set.empty
            (Just x) -> IO.withFile x IO.ReadMode $ \hRefIn ->
                fmap (Blacklist . Set.fromList . fmap (C.unpack . fastaSeq))
                    . readFasta
                    $ hRefIn

    let tempConfig = RepeatConfig
                        { _blacklist    = suppliedBlacklist
                        , _refMap       = refMap
                        , _richness     = Richness . minRichness $ opts
                        , _distance     = Distance 0
                        , _alphabet     = "ATCG"
                        , _minSize      = MinSize . inputMinSize $ opts
                        , _minMut       = fmap MinMut . inputMinMut $ opts
                        , _refCheckFlag = refCheckBlacklistFlag opts
                        }
        finalBlacklist =
            if refRecBlacklistFlag opts
                then refRecBlacklist tempConfig
                   . Map.elems
                   . unReferenceMap
                   . fromMaybe (error "No reference supplied.")
                   $ refMap
                else suppliedBlacklist
        config    = set distance (Distance . inputDistance $ opts)
                  . set blacklist finalBlacklist
                  $ tempConfig
        getDup fs = ( join
                    . fmap ( longestRepeatedSubstringMutations config []
                           . Query
                           . fastaSeq
                           )
                        . maybe (Just fs) (flip ignore fs)
                        . ignoreField
                        $ opts
                    , fs
                    )
        getSpace Nothing (!dup, !fs) =
            ( ITD { _duplication = fmap unLongestSubstring dup
                  , _spacer      = Nothing
                  }
            , fs
            )
        getSpace (Just rMap) (!dup, !fs) =
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
              refSeq = getReferenceSeq (refField opts) fs rMap
        getClass (!itd, !fs)     = (classifyITD itd, itd, fs)
        addPos fs                =
            maybe (Position 1) (\ x -> Position
                                        . read
                                        . C.unpack
                                        . fromMaybe ( error
                                                    $ "Position field out of bounds (use another field number): "
                                                   <> (C.unpack . fastaHeader $ fs)
                                                    )
                                        . (flip atMay (x - 1))
                                        . C.split '|'
                                        . fastaHeader $ fs
                                )
                . posField
                $ opts
        printRow (!c, !itd, !fs) =
            printITD
                (Label . C.pack . outputLabel $ opts)
                (addPos fs)
                fs
                c
                itd
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

    runEffect $ ( ( P.zip (Pipes.each [1..])
                        ( pipesFasta (PB.fromHandle hIn)
                        >-> P.map ( getSpace ( bool Nothing refMap
                                             $ spacerFlag opts
                                             )
                                  . getDup
                                  )
                        )
                    )
                >-> P.mapM (plotITDM opts)
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
