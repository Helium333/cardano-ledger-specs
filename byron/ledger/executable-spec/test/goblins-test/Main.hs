{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Concurrent.Async (async, wait)
import           Control.Monad (forM, forM_, replicateM_, unless)
import           Data.List (partition)
import           Data.TreeDiff.Class
import           Data.TreeDiff.Expr
import qualified Data.TypeRepMap as TM
import qualified Options.Applicative as O
import           System.FilePath.Posix ((</>), (<.>), makeValid)
import           System.Directory (createDirectoryIfMissing, doesFileExist)
import           System.Exit (exitFailure)
import           System.IO (writeFile)
import           Text.PrettyPrint (render)
import           Text.Read (readEither)

import           Cardano.Ledger.Spec.STS.UTXO (UTXO, PredicateFailure(..))
import           Cardano.Ledger.Spec.STS.UTXOW (UTXOW, PredicateFailure(..))
import           Cardano.Ledger.Spec.STS.UTXOWS (UTXOWS, PredicateFailure(..))
import           Control.State.Transition (Signal(..))
import           Control.State.Transition.Generator (HasTrace(..))
import           Control.State.Transition.Goblin.BreedingPit (breedStsGoblins, genBlarg, Gen, Population)
import           Ledger.Delegation (DELEG, ADELEG, ADELEGS, SDELEG, SDELEGS, PredicateFailure(..))


import           Ledger.Core
  ( Addr(..)
  , Epoch(..)
  , Hash(..)
  , Lovelace(..)
  , Owner(..)
  , Sig(..)
  , VKey(..)
  , VKeyGenesis(..)
  )
import           Ledger.Delegation
  (DCert(..)
  )
import           Ledger.UTxO
  ( Tx(..)
  , TxId(..)
  , TxIn(..)
  , TxOut(..)
  , TxWits(..)
  , Wit(..)
  )

import           Test.Goblin
import           Test.Goblin.Explainer

allRunsDir :: FilePath
allRunsDir = "runs"

main :: IO ()
main = do
  runCount <- O.execParser opts
  putStrLn ("Preparing for " <> show runCount <> " runs.")
  createDirectoryIfMissing True allRunsDir
  replicateM_ runCount trainGoblins

trainGoblins :: IO ()
trainGoblins = do
  -- Set up directories.
  let runIxFile = allRunsDir </> "count" <.> "txt"
  runIx <- parseRunIndexFromFile runIxFile
  let runDir    = allRunsDir </> show runIx
      genomeDir = runDir </> "goblin_genomes"
      passDir   = genomeDir </> "pass"
      failDir   = genomeDir </> "fail"
  mapM_ (createDirectoryIfMissing True) [passDir, failDir]

  let teeIt = tee (runDir </> "log" <.> "txt")
  teeIt ("Run index: " <> show runIx)
  actions <- forM breeders $ \(PopStruct name action wrappedGenSigs) -> async $ do
    pop <- action
    pure (pop, name, wrappedGenSigs)
  results <- traverse wait actions
  let (good, bad) = partition (\(pop,_,_) -> (snd (head pop)) > 1.0) results
  teeIt ("Total: " <> show (length results) <> ". Good: " <> show (length good) <> ". Bad: " <> show (length bad))
  forM_ good $ \(pop, name, _) -> do
    teeIt ("PASS: " <> name <> " generated goblins. Best score: " <> show (snd (head pop)) <> ".")
    writeFile (nameToPath passDir name) (show pop)
  forM_ bad $ \(pop, name, _) -> do
    writeFile (nameToPath failDir name) (show pop)
  teeIt ""

nameToPath :: FilePath -> String -> FilePath
nameToPath dir name = dir </> makeValid (subSpaces name)
 where
  subSpaces = subChar ' ' '_'
  subChar target replacement [] = []
  subChar target replacement (c:cs)
    | target == c = replacement : (subChar target replacement cs)
    | otherwise   = c           : (subChar target replacement cs)

tee :: FilePath -> String -> IO ()
tee fp msg = do
  putStrLn msg
  appendFile fp (msg++"\n")

  -- forM_ bad $ \(pop, name, _) ->
  --   putStrLn ("FAIL: " <> name <> " failed to produce good results. Score: " <> show (snd (head pop)) <> ".")
         -- putStrLn $ "Top 5: "
         -- mapM_ print (take 5 pop)
         -- let best = head pop
         -- let goblin = (spawnGoblin (fst best) TM.empty)
         -- let diffs = case wrappedGenSigs of
         --               WrapDELEG  genSigs -> explainTheGoblin @DELEG  genSigs goblin
         --               WrapUTXOW  genSigs -> explainTheGoblin @UTXOW  genSigs goblin
         --               WrapUTXOWS genSigs -> explainTheGoblin @UTXOWS genSigs goblin
         -- forM_ diffs $ \diff ->
         --   putStrLn $ render $ prettyEditExprCompact diff


explainTheGoblin :: forall sts
                  . (Goblin Bool (Signal sts), ToExpr (Signal sts))
                 => [Gen (Signal sts)]
                 -> GoblinData Bool
                 -> [Edit EditExpr]
explainTheGoblin genSigs goblin =
  map (\g -> maybe (error "explainTheGoblin: got Nothing") (\(_,_,diff,_) -> diff) (explainGoblinGen Nothing Nothing g goblin))
      genSigs


breedType :: forall sts
           . (Goblin Bool (Signal sts), HasTrace sts)
          => ([Gen (Signal sts)] -> WrappedGenSigs)
          -> PredicateFailure sts
          -> ( IO (Population Bool)
             , WrappedGenSigs
             )
breedType wrapper predicateFailure =
  ( do { vs <- breedStsGoblins @sts predicateFailure; writeFile "/dev/null" (show (snd (head vs))); pure vs }
  , wrapper genSigs
  )
 where
  genSigs = (snd <$>) <$> (genBlarg @sts)


{-
HasTrace UPIREG        ./byron/ledger/executable-spec/src/Ledger/Update.hs     846
HasTrace UPIVOTES      ./byron/ledger/executable-spec/src/Ledger/Update.hs     1224

-- this is massive - the toplevel STS, more or less
HasTrace CHAIN         ./byron/chain/executable-spec/src/Cardano/Spec/Chain/STS/Rule/Chain.hs  168

-- this is an example, don't need to check
-- HasTrace SUM           ./byron/semantics/executable-spec/test/Control/State/Transition/Examples/Sum.hs 38

  -- this is in a test package, which will need to be factored out
  -- HasTrace DBLOCK        ./byron/ledger/executable-spec/test/Ledger/Delegation/Properties.hs     285
  , map (breedType WrapDBLOCK)
        ( NotIncreasingBlockSlot
        : (map DPF delegPFs))

  -- ^ same for this
  -- HasTrace UBLOCK        ./byron/ledger/executable-spec/test/Ledger/Update/Properties.hs 358
-}

data PopStruct = PopStruct
  { popStructName :: String
  , popStructPop  :: IO (Population Bool)
  , popStructSigs :: WrappedGenSigs
  }


breeders :: [PopStruct]
breeders = concat $
  -- HasTrace DELEG         ./byron/ledger/executable-spec/src/Ledger/Delegation.hs 565
  [ map (\pf -> uncurry (PopStruct ("DELEG: " <> show pf)) (breedType WrapDELEG pf))
        delegPFs
  -- HasTrace UTXOW         ./byron/ledger/executable-spec/src/Cardano/Ledger/Spec/STS/UTXOW.hs     110
  , map (\pf -> uncurry (PopStruct ("UTXOW: " <> show pf)) (breedType WrapUTXOW pf))
        utxoPFs
  -- -- HasTrace UTXOWS        ./byron/ledger/executable-spec/src/Cardano/Ledger/Spec/STS/UTXOWS.hs    64
  -- , map (\pf -> uncurry (PopStruct ("UTXOWS: " <> show pf)) (breedType WrapUTXOWS pf))
  --       (map UtxowFailure utxoPFs)
  ]
 where
  delegPFs = (concat [ (map (SDelegSFailure . SDelegFailure)
                            [ IsNotGenesisKey
                            , EpochInThePast
                            , EpochPastNextEpoch
                            , HasAlreadyDelegated
                            , IsAlreadyScheduled
                            , DoesNotVerify
                            ])
                     , (map (ADelegSFailure . ADelegFailure)
                            [ BeforeExistingDelegation
                            , NoLastDelegation
                            , AfterExistingDelegation
                            , AlreadyADelegateOf (VKey (Owner 1)) (VKeyGenesis (VKey (Owner 2)))
                            ])
                     ])
  utxoPFs = ([ InsufficientWitnesses
             ] ++ (map UtxoFailure
                       [ EmptyTxInputs
                       , EmptyTxOutputs
                       , FeeTooLow
                       , IncreasedTotalBalance
                       , InputsNotInUTxO
                       , NonPositiveOutputs
                       ]))

-- I believe this is necessary to hide the differing STS types in the list
-- and appease the typechecker.
data WrappedGenSigs where
  WrapDELEG  :: [Gen (Signal DELEG)]  -> WrappedGenSigs
  WrapUTXOW  :: [Gen (Signal UTXOW)]  -> WrappedGenSigs
  WrapUTXOWS :: [Gen (Signal UTXOWS)] -> WrappedGenSigs


-- | Top level parser with info.
opts :: O.ParserInfo Int
opts = O.info (parseRunCount O.<**> O.helper)
    (  O.fullDesc
    <> O.progDesc "Goblins breeding executable."
    )

parseRunCount :: O.Parser Int
parseRunCount =
    O.option O.auto (
            O.long "run_count"
         <> O.short 'r'
         <> O.metavar "COUNT"
         <> O.value 1
         <> O.help "The desired number of runs. Each run trains goblins for each PredicateFailure of each STS rule"
    )

parseRunIndexFromFile :: FilePath -> IO Int
parseRunIndexFromFile fp = do
  doesExist <- doesFileExist fp
  unless doesExist $ do
    putStrLn "Initializing run index file."
    writeFile fp "0" -- initialize
  eVal <- readEither <$> readFile fp
  ix <- case eVal of
    Left err -> putStrLn ("parseRunIndexFromFile: no parse: " <> err)
             >> exitFailure
    Right ix -> pure ix
  writeFile fp (show (ix+1)) -- increment counter
  pure ix

  -- case (filter ((> 100.0) . snd) pop) of
  --   (best:_) -> do
  --     putStrLn $ "Best goblin scored " ++ show (snd best)
  --     -- let (Just diff) = explainGoblinGen (fmap thd jcGen) (spawnGoblin (fst best) TM.empty)
  --     -- putStrLn $ render $ prettyEditExprCompact diff
  --   [] -> do
  --     putStrLn "No good goblins bred!"

{-
:set -XTypeApplications
import Data.Maybe
import System.Random
import qualified Hedgehog as H
import qualified Hedgehog.Internal.Seed as Seed
import Test.Goblin
import Test.Goblin.Explainer
import           Text.PrettyPrint (render)
import Instances
import qualified Data.TypeRepMap as TM
import Ledger.Delegation
import Control.State.Transition.Goblin.BreedingPit
let dcertGens = map ((head . snd) <$>) (genBlarg @DELEG)
(stdGen1, stdGen2) <- split <$> getStdGen
let genome = take 100 $ randoms stdGen1
setStdGen stdGen2
let goblinData = spawnGoblin genome TM.empty
let size = H.Size 10000
seed <- Seed.random
mapM_ (\gen -> putStrLn (render (prettyEditExprCompact (fromJust (explainGoblinGen (Just size) (Just seed) gen goblinData))))) dcertGens
-}
