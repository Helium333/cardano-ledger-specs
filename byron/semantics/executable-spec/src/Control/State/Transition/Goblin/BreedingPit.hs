{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Control.State.Transition.Goblin.BreedingPit
  (
    breedStsGoblins
  )
where

import           Control.Monad (forM, join)
import           Control.Monad.State.Strict (evalStateT)
import           Control.Monad.Trans.Maybe (runMaybeT)
import qualified Data.TypeRepMap as TM
import           Hedgehog
import qualified Hedgehog.Internal.Gen as IGen
import qualified Hedgehog.Internal.Tree as ITree
import qualified Hedgehog.Range as Range

import           Control.State.Transition
  (IRC(..), STS(..), TRC(..), applyRuleIndifferently)
import           Control.State.Transition.Generator (HasTrace(..))
import           Test.Goblin
import           Moo.GeneticAlgorithm.Binary

breedStsGoblins
  :: forall sts
   . (HasTrace sts, Goblin Bool (Signal sts))
  => PredicateFailure sts
  -> IO ()
breedStsGoblins wantedFailure =
  let
    popsize    = 101
    genomeSize = 100
    maxiters   = 10000

    genSize    = Range.Size 1
    genSeed    = Seed 12345 12345

    -- TODO @mhueschen: unclear to me whether the environment should stay the same
    -- or change over the duration of a breeding cycle.

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
          let gd = spawnGoblin genome TM.empty
          --
          env <- initEnvGen @sts
          case applyRuleIndifferently @sts (IRC env) <$> initialRules of
            [] -> error "no initStates"
            ps -> let pfs :: [PredicateFailure sts]
                      pfs = join (snd <$> ps)
                   in if not (null pfs)
                         then pure (scoreResult pfs)
                         else do
                           let initStates = map fst ps

                           sigs <- forM initStates (sigGen @sts env)
                           newSigs <- forM sigs
                                        (\sig -> evalStateT (tinker sig) gd)

                           let jcs = [ TRC (env, st, newSig)
                                     | st <- initStates
                                     , newSig <- newSigs
                                     ]

                           -- Apply the signal to the state (and environment)
                           let results = [ applyRuleIndifferently @sts jc trs
                                         | jc <- jcs
                                         , trs <- transitionRules
                                         ]

                           -- Score the result
                           pure $ scoreResult (join (snd <$> results))
     where

      scoreResult :: [PredicateFailure sts] -> Double
      scoreResult failures =
        -- 0 points for a rule passing
        -- -1 points for an unwanted predicate failure
        -- 5 points for a desired predicate failure
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
