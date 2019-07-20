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
import           Data.Data (Data, toConstr)
import qualified Data.TypeRepMap as TM
import           Hedgehog
import qualified Hedgehog.Internal.Gen as IGen
import qualified Hedgehog.Internal.Seed as Seed
import qualified Hedgehog.Internal.Tree as ITree
import qualified Hedgehog.Range as Range

import           Control.State.Transition
  (IRC(..), STS(..), TRC(..), applyRuleIndifferently)
import           Control.State.Transition.Generator (HasTrace(..))
import           Test.Goblin
import           Moo.GeneticAlgorithm.Binary

breedStsGoblins
  :: forall sts
   . ( HasTrace sts, Goblin Bool (Signal sts), Data (PredicateFailure sts)
     , SeedGoblin (Environment sts), SeedGoblin (State sts))
  => PredicateFailure sts
  -> IO (Population Bool)
breedStsGoblins wantedFailure = do
  genSeed <- Seed.random

  let
    popsize    = 500
    genomeSize = 4000
    maxiters   = 100
    eliteCount = 5

    genSize    = Range.Size 50

    -- TODO @mhueschen: unclear to me whether the environment should stay the same
    -- or change over the duration of a breeding cycle.

    -- | Fitness function. This should run the goblins on a set of examples
    -- which we generate atop `initState`.
    fitness :: [Bool] -> Double
    fitness genome = scoreResult $ do -- this is a List monad
      blarg <- genBlarg @sts
      let env   :: Environment sts
          state :: State sts
          (env, state) = maybe (error "wat") id
                       $ ITree.treeValue
                       . runMaybeT
                       . distributeT
                       . IGen.runGenT genSize genSeed
                       $ (fst <$> blarg)

      -- Seed the bagOfTricks
      let seedBagOfTricks = seeder env >> seeder state

      let gd = spawnGoblin genome TM.empty
      let newGenSig = flip evalState gd $ do
                        seedBagOfTricks
                        tinker (snd <$> blarg)

      let newSig :: Signal sts
          newSig = maybe (error "wat") id
                 $ ITree.treeValue
                 . runMaybeT
                 . distributeT
                 . IGen.runGenT genSize genSeed
                 $ newGenSig

      let jc = TRC (env, state, newSig)

      -- Apply the signal to the state (and environment)
      tr <- transitionRules
      let (_finalState, pfs) = applyRuleIndifferently @sts jc tr

      pure pfs

     where

      scoreResult :: [[PredicateFailure sts]] -> Double
      scoreResult ls =
        -- 1 for a rule passing
        -- 1 for an unwanted PredicateFailure
        -- 6 for a desired PredicateFailure
        -- --
        -- ^ this objective function must be positive, so
        -- we can't punish unwanted `PredicateFailure`s
        let goodFailuresCount =
              fromIntegral (length (filter (\pf -> toConstr pf == toConstr wantedFailure)
                                           (concat ls)))
        in 1 + (5 * goodFailuresCount)

    initialize = getRandomBinaryGenomes popsize genomeSize
    select     = stochasticUniversalSampling popsize
    crossover  = onePointCrossover 0.5
    mutate     = pointMutate 0.01
    -- evolve     = loopIO [ TimeLimit 30
    --                     -- , DoEvery 1 (\n pop -> putStrLn $ "gen: " <> show n <> ". " <> show (map snd pop))
    --                     ]
    evolve     = loop (Generations maxiters)-- `Or` converged)
      $ nextGeneration Maximizing fitness select eliteCount crossover mutate
     where
      -- converged =
      --   IfObjective $ \fitvals -> maximum fitvals == minimum fitvals
  population <- runGA initialize evolve
  pure (bestFirst Maximizing population)



genBlarg :: forall sts
          . HasTrace sts
         => [Gen ((Environment sts, State sts), Signal sts)]
genBlarg = do -- below is a List monad
  initRule <- initialRules
  pure $ do
    -- below is a Gen monad
    env <- envGen @sts 3
    let (initState, _predicateFailures) =
          applyRuleIndifferently @sts (IRC env) initRule

    sig <- sigGen @sts Nothing env initState
    pure ((env, initState), sig)
