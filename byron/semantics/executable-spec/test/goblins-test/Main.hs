{-# LANGUAGE TypeApplications #-}

-- import           Data.TreeDiff.Pretty
-- import qualified Data.TypeRepMap as TM
-- import           Text.PrettyPrint (render)

import qualified Cardano.Ledger.Spec.STS.UTXOW as U (UTXOW, PredicateFailure(..))
-- import           Control.State.Transition.Generator (HasTrace(..))
import           Control.State.Transition.Goblin.BreedingPit (breedStsGoblins)
import           Instances ()
-- import           Test.Goblin
-- import           Test.Goblin.Explainer

main :: IO ()
main = do
  pop <- breedStsGoblins @U.UTXOW U.InsufficientWitnesses
  case (filter ((> 100.0) . snd) pop) of
    (best:_) -> do
      putStrLn $ "Best goblin scored " ++ show (snd best)
      -- let (Just diff) = explainGoblinGen (fmap thd jcGen) (spawnGoblin (fst best) TM.empty)
      -- putStrLn $ render $ prettyEditExprCompact diff
    [] -> putStrLn "No good goblins bred!"

thd :: (a,b,c) -> c
thd (_,_,x) = x
