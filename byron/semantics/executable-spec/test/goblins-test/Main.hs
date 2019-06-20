{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans  #-}
import           Control.Monad (forM_)
import           Data.TreeDiff.Class
import           Data.TreeDiff.Expr
import           Data.TreeDiff.Pretty
import qualified Data.TypeRepMap as TM
import           Text.PrettyPrint (render)

import qualified Cardano.Ledger.Spec.STS.UTXOW as U (UTXOW, PredicateFailure(..))
-- import           Control.State.Transition.Generator (HasTrace(..))
import           Control.State.Transition.Goblin.BreedingPit (breedStsGoblins, genBlarg)
import           Instances ()
import           Ledger.Delegation (DELEG, PredicateFailure(..))

import           Ledger.Core
  ( Addr(..)
  , Hash(..)
  , Lovelace(..)
  , Owner(..)
  , Sig(..)
  , VKey(..)
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

bar = breedStsGoblins @DELEG (SDelegSFailure (SDelegFailure IsAlreadyScheduled))

main :: IO ()
main = do
  pop <- breedStsGoblins @U.UTXOW U.InsufficientWitnesses
  putStrLn $ "Top 5: "
  mapM_ print (take 5 pop)
  let best = head pop
  let diffs = map (\g -> explainGoblinGen g (spawnGoblin (fst best) TM.empty))
                  genSigs
  forM_ diffs $ \(Just diff) ->
    putStrLn $ render $ prettyEditExprCompact diff

 where
  genSigs = (snd <$>) <$> (genBlarg @U.UTXOW)

instance ToExpr Addr where
instance ToExpr Lovelace where
  toExpr (Lovelace x) = App "Lovelace" [toExpr x]
instance ToExpr Owner where
  toExpr (Owner x) = App "Owner" [toExpr x]
instance ToExpr (Sig Tx)
instance ToExpr Tx
instance ToExpr TxId where
  toExpr (TxId (Hash i)) = App "TxId" [App "Hash" [toExpr i]]
instance ToExpr TxIn
instance ToExpr TxOut
instance ToExpr TxWits
instance ToExpr VKey where
  toExpr (VKey x) = App "VKey" [toExpr x]
instance ToExpr Wit

  -- case (filter ((> 100.0) . snd) pop) of
  --   (best:_) -> do
  --     putStrLn $ "Best goblin scored " ++ show (snd best)
  --     -- let (Just diff) = explainGoblinGen (fmap thd jcGen) (spawnGoblin (fst best) TM.empty)
  --     -- putStrLn $ render $ prettyEditExprCompact diff
  --   [] -> do
  --     putStrLn "No good goblins bred!"
