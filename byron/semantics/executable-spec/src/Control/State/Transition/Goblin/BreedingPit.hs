{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Control.State.Transition.Goblin.BreedingPit
  (
    breedStsGoblins
  , genBlarg
  , Gen
  , Population
  )
where

import           Control.Monad.State.Strict (evalState)
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
  -> IO (Population Bool)
breedStsGoblins wantedFailure =
  let
    popsize    = 500
    genomeSize = 1000
    maxiters   = 5

    genSize    = Range.Size 1
    genSeed    = Seed 12345 12345

    -- TODO @mhueschen: unclear to me whether the environment should stay the same
    -- or change over the duration of a breeding cycle.

    -- | Fitness function. This should run the goblins on a set of examples
    -- which we generate atop `initState`.
    fitness :: [Bool] -> Double
    fitness genome = scoreResult $ do -- this is a List monad
      let gd = spawnGoblin genome TM.empty

      blarg <- genBlarg @sts
      let newGenSig = evalState (tinker (snd <$> blarg)) gd

      let newSig :: Signal sts
          newSig = maybe (error "wat") id
                 $ ITree.treeValue
                 . runMaybeT
                 . distributeT
                 . IGen.runGenT genSize genSeed
                 $ newGenSig

      let envState :: (Environment sts, State sts)
          envState = maybe (error "wat") id
                   $ ITree.treeValue
                   . runMaybeT
                   . distributeT
                   . IGen.runGenT genSize genSeed
                   $ (fst <$> blarg)
                   -- TODO mhueschen | ^ does this make sense? should we
                   -- tinker with both the env & state & the sig?

      let jc = TRC (fst envState, snd envState, newSig)

      -- Apply the signal to the state (and environment)
      tr <- transitionRules
      let (_finalState, pfs) = applyRuleIndifferently @sts jc tr

      pure pfs

     where

      scoreResult :: [[PredicateFailure sts]] -> Double
      scoreResult ls =
        -- 0 for a rule passing
        -- 0 for an unwanted PredicateFailure
        -- 5 for a desired PredicateFailure
        -- --
        -- ^ this objective function must be positive, so
        -- we can't punish unwanted `PredicateFailure`s
        let failures = concat ls
            goodFailuresCount =
              fromIntegral (length (filter (== wantedFailure)
                                           failures))
        in (5 * goodFailuresCount)

    initialize = getRandomBinaryGenomes popsize genomeSize
    select     = stochasticUniversalSampling popsize
    crossover  = onePointCrossover 0.5
    mutate     = pointMutate 0.01
    -- evolve     = loopIO [ TimeLimit 30
    --                     -- , DoEvery 1 (\n pop -> putStrLn $ "gen: " <> show n <> ". " <> show (map snd pop))
    --                     ]
    evolve     = loop (Generations maxiters)-- `Or` converged)
      $ nextGeneration Maximizing fitness select 0 crossover mutate
     where
      -- converged =
      --   IfObjective $ \fitvals -> maximum fitvals == minimum fitvals
  in do
    population <- runGA initialize evolve
    pure (bestFirst Minimizing population)



genBlarg :: forall sts
          . HasTrace sts
         => [Gen ((Environment sts, State sts), Signal sts)]
genBlarg = do -- below is a List monad
  initRule <- initialRules
  pure $ do
    -- below is a Gen monad
    env <- initEnvGen @sts
    let (initState, _predicateFailures) =
          applyRuleIndifferently @sts (IRC env) initRule

    sig <- sigGen @sts env initState
    pure ((env, initState), sig)