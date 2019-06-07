{-# LANGUAGE TypeApplications #-}

import           Data.TreeDiff.Pretty
import qualified Data.TypeRepMap as TM
import           Text.PrettyPrint (render)

import           Cardano.Ledger.Spec.STS.UTXOW (UTXOW(..), PredicateFailure(..))
import           Control.State.Transition (STS(..))
import           Control.State.Transition.Goblin.BreedingPit (breedStsGoblins)
import           Test.Goblin
import           Test.Goblin.Explainer

main :: IO ()
main = do
  pop <- breedStsGoblins @UTXOW InsufficientWitnesses
  case (filter ((> 100.0) . snd) pop) of
    (best:_) -> do
      putStrLn $ "Best goblin scored " ++ show (snd best)
      -- let (Just diff) = explainGoblinGen (fmap thd jcGen) (spawnGoblin (fst best) TM.empty)
      -- putStrLn $ render $ prettyEditExprCompact diff
    [] -> putStrLn "No good goblins bred!"

thd :: (a,b,c) -> c
thd (_,_,x) = x
