{-# LANGUAGE TypeApplications #-}
import Control.State.Transition.Goblin.BreedingPit (breedStsGoblins)
import Test.Goblin.Explainer
import Test.Goblin
import Data.TreeDiff.Pretty
import           Generator
import LedgerState
import Slot
import qualified Data.TypeRepMap as TM
import Control.Lens ((^.), _1, _2, _3, view)
import Text.PrettyPrint (render)

main :: IO ()
main = do
    pop <- breedStsGoblins jcGen (UTXOFailure [StakeKeyAlreadyRegistered])
    case (filter ((> 100.0) . view _2) pop) of
      (best:_) -> do
        putStrLn $ "Best goblin scored " ++ show (best ^. _2)
        let (Just diff) = explainGoblinGen (fmap (view _3) jcGen) (spawnGoblin (best ^. _1) TM.empty)
        putStrLn $ render $ prettyEditExprCompact diff
      [] -> putStrLn "No good goblins bred!"
  where
    jcGen = do
      (_, steps, _, sig, ls) <- genValidStateTx
      return (Slot steps, ls, sig)
