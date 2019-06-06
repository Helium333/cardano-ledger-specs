{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Control.State.Transition.Goblin.BreedingPit
  (
    breedStsGoblins
  )
where

import Control.Monad (join)
import Control.Monad.State.Strict (evalStateT)
import Control.Monad.Trans.Maybe (runMaybeT)
import qualified Data.TypeRepMap as TM
import Hedgehog
import qualified Hedgehog.Internal.Gen as IGen
import qualified Hedgehog.Internal.Tree as ITree
import qualified Hedgehog.Range as Range

import Control.State.Transition
import Test.Goblin
import Moo.GeneticAlgorithm.Binary

breedStsGoblins
  :: forall s
   . (STS s, Goblin Bool (Signal s))
  => Environment s -- ^ Rule environment
  -> State s -- ^ Initial valid state
  -> Gen (Signal s) -- ^ Generator for a new signal
  -> PredicateFailure s -- ^ Failure we are looking for
  -> IO ()
breedStsGoblins env initState sigGen wantedFailure =
  let
    popsize    = 101
    genomeSize = 100
    maxiters   = 10000

    genSize    = Range.Size 1
    genSeed    = Seed 12345 12345

    -- | Fitness function. This should run the goblins on a set of examples
    -- which we generate atop `initState`.
    fitness :: [Bool] -> Double
    fitness genome =
      maybe 0 id
        . ITree.treeValue
        . runMaybeT
        . distributeT
        . IGen.runGenT genSize genSeed
        $ do
        -- TODO Loop some number of times
            sig    <- sigGen
            newSig <- evalStateT (tinker sig) gd
            -- Apply the signal to the state (and environment)
            let
              jc      = TRC (env, initState, newSig)
              results = applyRuleIndifferently @s jc <$> transitionRules
            -- Score the result
            return $ scoreResult results
     where
      gd = spawnGoblin genome TM.empty
      scoreResult :: [(State s, [PredicateFailure s])] -> Double
      scoreResult result =
        -- 0 points for a rule passing
        -- -1 points for an unwanted predicate failure
        -- 5 points for a desired predicate failure
        let failures = join $ snd <$> result
        in
          fromIntegral
          $ (5 * length (filter (== wantedFailure) failures))
          - length failures

    initialize = getRandomBinaryGenomes popsize genomeSize
    select     = stochasticUniversalSampling popsize
    crossover  = onePointCrossover 0.5
    mutate     = pointMutate 0.01
    evolve     = loop (Generations maxiters `Or` converged)
      $ nextGeneration Maximizing fitness select 0 crossover mutate
     where
      converged =
        IfObjective $ \fitvals -> maximum fitvals == minimum fitvals
  in do
    population <- runGA initialize evolve
    print (bestFirst Minimizing population)
