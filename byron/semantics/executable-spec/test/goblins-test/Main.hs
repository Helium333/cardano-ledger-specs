{-# LANGUAGE TypeApplications #-}

import           Data.TreeDiff.Pretty
import qualified Data.TypeRepMap as TM
import           Text.PrettyPrint (render)

import           Control.State.Transition.Goblin.BreedingPit (breedStsGoblins)
import           Test.Goblin
import           Test.Goblin.Explainer

main :: IO ()
main = do
  pop <- breedStsGoblins sigGen (UTXOFailure [StakeKeyAlreadyRegistered])
  case (filter ((> 100.0) . view _2) pop) of
    (best:_) -> do
      putStrLn $ "Best goblin scored " ++ show (best ^. _2)
      let (Just diff) = explainGoblinGen (fmap (view _3) jcGen) (spawnGoblin (best ^. _1) TM.empty)
      putStrLn $ render $ prettyEditExprCompact diff
    [] -> putStrLn "No good goblins bred!"
 where
  startState = unwrapRule (head initialRules)
  jcGen = do
    (_, steps, _, sig, ls) <- genValidStateTx
    return (Slot steps, ls, sig)
